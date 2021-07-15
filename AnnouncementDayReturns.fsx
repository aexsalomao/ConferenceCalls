#r "nuget: FSharp.Data, 4.1.1"
#r "nuget: Newtonsoft.Json, 13.0.1"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#load "PagePParsing.fsx"
#load "secrets.fsx"
#load "Common.fsx"

open System
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET

open PagePParsing
open Secrets
open Common

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
# Read transcripts
*)

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)
    
let allTranscripts = readJson ("data-cache/EarningsCallTest200.json")

(**
# Tiingo

- Before actually making any Tiingo calls I thought it would be good idea
  to check if they actually support that ticker ...
*)

type TiingoSupportedTickersCsv = CsvProvider<Sample="data-cache/SupportedTiingoTickers.csv",
                                             ResolutionFolder = __SOURCE_DIRECTORY__>
                                           
let supportedTiingoTickers = TiingoSupportedTickersCsv.GetSample()

// Filtering 'allTranscripts'

type SupportedTicker = {Exchange : string; Ticker : string}

let tiingoTickers = 
    supportedTiingoTickers.Rows
    |> Seq.map (fun xs ->
    let ex = xs.Exchange.ToString().Trim()
    let tckr = xs.Ticker.ToString().Trim()
    {Exchange = ex
     Ticker = tckr})

let transcriptTickers = 
    allTranscripts 
    |> Seq.map (fun x -> (x.Exchange, x.Ticker), x) 
    |> Map

let myTranscripts =
    tiingoTickers
    |> Seq.choose (fun x -> 
    match transcriptTickers.TryFind(x.Exchange, x.Ticker) with
    | Some xs -> Some xs
    | None -> None)

(**
# Calls by time of day barplot
- Eastern Time (ET)
*)

let callsByTimeOfDay = 
    myTranscripts
    |> Seq.countBy (fun x -> x.Date.Hour)
    |> Seq.sortBy (fun (hour, count) -> hour)
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by time of day (N: {myTranscripts |> Seq.length})"
    |> Chart.withX_AxisStyle "Hour"
    |> Chart.withY_AxisStyle "Count"
    |> Chart.Show

let callsByDay = 
    myTranscripts
    |> Seq.countBy (fun x -> x.Date.DayOfWeek, x.Date.DayOfWeek.ToString())
    |> Seq.sortBy fst
    |> Seq.map(fun ((dayInt, dayString), count) -> dayString, count )
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by day of week (N: {myTranscripts |> Seq.length})"
    |> Chart.withY_AxisStyle "Count"
    |> Chart.Show
  
(**
# Market returns (SPY)
- Fetch market returns over the period of interest
- Prevents multiple tiingo calls
*)

let startSample = myTranscripts |> Seq.minBy (fun x -> x.Date) |> fun x -> x.Date.AddDays(-10.)
let endSample = myTranscripts |> Seq.maxBy (fun x -> x.Date) |> fun x -> x.Date.AddDays(+10.)

let spyReturnsMap =
    "SPY"
    |> Tiingo.request
    |> Tiingo.startOn startSample
    |> Tiingo.endOn endSample
    |> Tiingo.getReturns
    |> Seq.map (fun x -> x.Date, x)
    |> Map

(**
# Splitting observations

- Before market close
- After market close
*)

(**
# Announcement Day Returns
*)

type AnnouncementWindow = 
    { Transcript : Transcript 
      TradingDayAfterCall : DateTime }

// Before Mkt Close Example
let allBeforeMktObs = myTranscripts |> Seq.filter (fun x -> x.Date.Hour < 16)
let obsBefore = allBeforeMktObs |> Seq.filter (fun x -> x.Ticker = "AAP") |> Seq.exactlyOne

// After Mkt Close Example
let allAfterMktObs = myTranscripts |> Seq.filter (fun x -> x.Date.Hour >= 16)
let obsAfter = allAfterMktObs |> Seq.head

(**
# Returns Around Earnings Announcements
*)

// Types
type ReturnsWindow = 
    {Transcript : Transcript
     ReturnsAroundEarnings : ReturnObs []
     
        member this.CumulativeReturnn = 
            this.ReturnsAroundEarnings
     }

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float}

// (1)

let TwoWeekReturnWindow (obs: Transcript): ReturnsWindow option =
    obs.Ticker
    |> Tiingo.request
    |> Tiingo.startOn (obs.Date.AddDays(-7.0))
    |> Tiingo.endOn (obs.Date.AddDays(7.0))
    |> Tiingo.getReturns
    |> fun xs -> 
    match xs with
    | [||] -> None
    | _ -> Some {Transcript = obs
                 ReturnsAroundEarnings = xs}

// (2)

let dayOfEarningsAnnouncement (obs: Transcript): DateTime =
    let onlyDate = obs.Date
    if onlyDate.Hour < 16 then onlyDate.Date else onlyDate.Date.AddDays(1.0)

let postEarningsReturn (obs: ReturnsWindow): ReturnObs option =
    let eaDay = obs.Transcript |> dayOfEarningsAnnouncement

    obs.ReturnsAroundEarnings 
    |> Seq.tryFind (fun xs -> xs.Date.Date >= eaDay)

let ThreeDayWindow (obs: ReturnsWindow): ReturnObs [] option=
    let postEarningsReturn = obs |> postEarningsReturn

    let filteredObs = 
        obs.ReturnsAroundEarnings
        |> Array.filter (fun x -> spyReturnsMap |> Map.containsKey x.Date)

    filteredObs
    |> Array.windowed 3
    |> Array.filter (fun xs ->
        match postEarningsReturn, (xs |> Array.map (fun xs -> xs.Date)) with
        | Some retObs, [|d1; d2; d3|] -> retObs.Date = d2.Date.Date
        | _ -> failwith "why don't you have 3 items?")
    |> Array.tryExactlyOne

let ReturnsAroundEarningsAnnouncement (obs: ReturnsWindow option) = 
    match obs with 
    | None -> None
    | Some retWindow ->

        let retAroundEarnings = 
            retWindow 
            |> ThreeDayWindow

        Some {Transcript = retWindow.Transcript
              ReturnsAroundEarnings = retWindow.ReturnsAroundEarnings}

// (3)

let ExcessCumulativeReturnsFromSPY (obs: ReturnObs []) = 
    let adjRet = 
        obs
        |> Array.choose (fun x ->
        match spyReturnsMap.TryFind(x.Date) with
        | None -> None
        | Some spyObs -> Some {Symbol = x.Symbol
                               Date = x.Date
                               Return = x.Return - spyObs.Return})
    
    // Cumulative Return
    (1.0, adjRet)
    ||> Array.fold (fun acc x -> acc*(1.0+ x.Return))
    |> fun xs -> xs - 1.
    
let AdjustedCumulativeReturns (obs: ReturnsWindow option) =
    match obs with 
    | None -> None
    | Some retWindow ->

        let cumRet = retWindow.ReturnsAroundEarnings |> ExcessCumulativeReturnsFromSPY

        Some {Transcript = retWindow.Transcript
              CumulativeReturn = cumRet}

let resultss (obs: Transcript) =
    obs
    |> TwoWeekReturnWindow
    |> ReturnsAroundEarningsAnnouncement
    |> AdjustedCumulativeReturns

let t1000 = 
    myTranscripts
    |> Seq.take 1000
    |> Seq.choose resultss
    |> Seq.toArray

t1000 |> Seq.filter ( fun x-> x.CumulativeReturn >= 0.02) |> Seq.length
t1000 |> Seq.filter ( fun x-> x.CumulativeReturn <= -0.02) |> Seq.length

let myChart = 
    let cumRets = t1000 |> Seq.map (fun x -> x.CumulativeReturn)
    cumRets
    |> Chart.Histogram
    |> Chart.withTitle $"(cumulative) adjusted returns around earnings announcements (N: {cumRets |> Seq.length}/500)"
    |> Chart.withSize (750., 500.)
    |> Chart.Show