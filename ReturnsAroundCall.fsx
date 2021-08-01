#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#load "TranscriptParsing.fsx"
#load "Secrets.fsx"
#load "Common.fsx"

open System
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET

open TranscriptParsing
open Common

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
# Read transcripts
*)

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)
    
let transcripts = readJson ("data-cache/Transcripts3000.json") |> Seq.toArray

(**
# Calls by time of day barplot

**Eastern Time (ET)**
*)

let callsByTimeOfDay = 
    transcripts
    |> Seq.countBy (fun x -> x.Date.Hour)
    |> Seq.sortBy (fun (hour, count) -> hour)
    |> Chart.Column
    |> Chart.withTitle $"Earnings calls by time of day (N: {transcripts |> Seq.length})"
    |> Chart.withX_AxisStyle "Hour"
    |> Chart.withY_AxisStyle "Count"

let callsByWeekDay = 
    transcripts
    |> Seq.countBy (fun x -> x.Date.DayOfWeek, x.Date.DayOfWeek.ToString())
    |> Seq.sortBy fst
    |> Seq.map(fun ((dayInt, dayString), count) -> dayString, count )
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by week day (N: {transcripts |> Seq.length})"
    |> Chart.withY_AxisStyle "Count"
  
(**
# Market returns (SPY)
- Fetch market returns over the period of interest
- Prevents multiple tiingo calls
*)

let startSample = 
    transcripts 
    |> Seq.minBy (fun x -> x.Date) 
    |> fun x -> x.Date.AddDays(-10.)

let endSample = 
    transcripts 
    |> Seq.maxBy (fun x -> x.Date) 
    |> fun x -> x.Date.AddDays(+10.)

let spyReturnsMap =
    "SPY"
    |> Tiingo.request
    |> Tiingo.startOn startSample
    |> Tiingo.endOn endSample
    |> Tiingo.getReturns
    |> Seq.map (fun x -> x.Date, x)
    |> Map

(**
# Splitting observations
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
let allBeforeMktObs = transcripts |> Seq.filter (fun x -> x.Date.Hour < 16)
let obsBefore = allBeforeMktObs |> Seq.filter (fun x -> x.Ticker = "AAP") |> Seq.exactlyOne

// After Mkt Close Example
let allAfterMktObs = transcripts |> Seq.filter (fun x -> x.Date.Hour >= 16)
let obsAfter = allAfterMktObs |> Seq.head

(**
# Returns Around Earnings Announcements
*)

// Types
type ReturnsWindow = 
    {Transcript : Transcript
     ReturnsAroundEarnings : ReturnObs []}

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float}

let twoWeekReturnWindow (obs: Transcript): ReturnsWindow option =
    obs.Ticker
    |> Tiingo.request
    |> Tiingo.startOn (obs.Date.AddDays(-7.0))
    |> Tiingo.endOn (obs.Date.AddDays(7.0))
    |> Tiingo.getReturns
    |> fun returns -> 
    match returns with
    | [||] -> None
    | _ -> Some {Transcript = obs
                 ReturnsAroundEarnings = returns}

let postEarningsReturn (obs: ReturnsWindow): ReturnObs option =
    let eaDay = if obs.Transcript.Date.Hour < 16 then 
                    obs.Transcript.Date 
                else 
                    obs.Transcript.Date.Date.AddDays(1.0)

    obs.ReturnsAroundEarnings 
    |> Seq.tryFind (fun xs -> xs.Date.Date >= eaDay)

let threeDayWindow (obs: ReturnsWindow): ReturnObs [] option =
    let postEarningsReturn = 
        obs 
        |> postEarningsReturn

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

let returnsAroundEarningsAnnouncement (obs: ReturnsWindow option) = 
    match obs with 
    | None -> None
    | Some retWindow ->

        let retAroundEarnings = 
            retWindow 
            |> threeDayWindow

        Some {Transcript = retWindow.Transcript
              ReturnsAroundEarnings = retWindow.ReturnsAroundEarnings}

let excessCumulativeReturnsFromSPY (obs: ReturnObs []) = 
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
    
let adjustedCumulativeReturns (obs: ReturnsWindow option) =
    match obs with 
    | None -> None
    | Some retWindow ->

        let cumulativeReturn = retWindow.ReturnsAroundEarnings |> excessCumulativeReturnsFromSPY

        Some {Transcript = retWindow.Transcript
              CumulativeReturn = cumulativeReturn}

let getReturnsAroundAnnouncement (obs: Transcript) =
    obs
    |> twoWeekReturnWindow
    |> returnsAroundEarningsAnnouncement
    |> adjustedCumulativeReturns

(**
# Download and Export to json
*)

let AnnouncementDayReturnToJson (fileName: string) (transcripts: AnnouncementDayReturn []) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(**
let t2500 = 
    transcripts
    |> Seq.skip 500
    |> Seq.choose getReturnsAroundAnnouncement
    |> Seq.toArray
    |> AnnouncementDayReturnToJson "data-cache/ReturnsAroundCall2500.json"
*)