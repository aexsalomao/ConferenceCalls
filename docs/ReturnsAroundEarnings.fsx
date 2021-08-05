(**
---
title: Returns around earnings
category: Scripts
categoryindex: 2
index: 2
---
*)

(**
# Returns around earnings
*)

(**
In the `TranscriptParsing.fsx` script, we dowloaded company specific earnings call transcripts, 
ticker, exchange and the exact date and time of each earnings call. 

With this information, can use Tiingo's API to fetch end-of-day price data for each ticker around the date of the company's earnings call.
To be more precise, we will fetch end-of-day closing prices from a three-day window period. Why a three-day window?

Since earnings announcements are to a certain extent expected to have an impact on market prices, 
we have to be careful when computing returns as we would be computing returns from **end-of-day** prices. 
While some calls might happen before or during market hours, some calls happen after market hours and so we have to be careful when computing returns for different companies.

For any given earnings call, what we want is to compute returns from 3 closing prices: (1) the last closing price prior to the call (2) the closing price 
after the call took place, and finally (3) the closing price after the day of the call.

For example:

- If the call had occured before or even during market hours, say the call happened at 11AM ET on a Tuesday, we would fetch Monday's, Tuesday's, and Wednesday's end-of-day prices.
- Likewise, if the call had occured after market hours, say the call happened at 7PM ET on a Tuesday, we would fetch Tuesday's, Wedneday's, and Thursday's end-of-day prices.

From these price observations, we can compute the total return or cumualtive return of a specific company around its earnings announcement. 
*)

(**
## Importing packages and loading scripts
*)

#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#load "TranscriptParsing.fsx"
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
## Reading Json

Here we read the data that we had downloaded from the `TranscriptParsing.fsx` script. 
Note how F# already identifies it as a sequence of `Transcript` records.
*)

/// Reading JSON
let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)

/// Data
let myTranscripts = 
    readJson ("data-cache/Motley100.json")
    |> Seq.take 200
    |> Seq.toArray

(**
## Barplots: Timing of calls
*)

let callsByTimeOfDay (transcripts : Transcript []) = 
    transcripts
    |> Seq.countBy (fun x -> x.Date.Hour)
    |> Seq.sortBy (fun (hour, count) -> hour)
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by time of day (N: {Seq.length transcripts})"
    |> Chart.withX_AxisStyle "Hour"
    |> Chart.withY_AxisStyle "Count"

/// myTranscripts |> callsByTimeOfDay |> Chart.Show

let callsByDayofWeek (transcripts: Transcript []) = 
    transcripts
    |> Seq.countBy (fun x -> x.Date.DayOfWeek, x.Date.DayOfWeek.ToString())
    |> Seq.sortBy fst
    |> Seq.map(fun ((dayInt, dayString), count) -> dayString, count )
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by day of week (N: {Seq.length transcripts})"
    |> Chart.withY_AxisStyle "Count"

/// myTranscripts |> callsByDayofWeek |> Chart.Show

(**
## Tiingo returns

Provided that Tiingo supports the ticker we are looking for, we can use the `Tiingo` module from `Common.fsx` to download returns data for any given ticker.
To be extra careful, we can check if Tiingo is returning any data at all with some pattern matching.
*)

/// Tiingo returns call
let tiingoReturns (tiingoStart: DateTime)
                  (tiingoEnd: DateTime)
                  (ticker: string) =
    ticker
    |> Tiingo.request
    |> Tiingo.startOn tiingoStart
    |> Tiingo.endOn tiingoEnd
    |> Tiingo.getReturns
    |> fun tiingoRets ->
    match tiingoRets with
    | [||] -> None
    | _ -> Some (tiingoRets)

/// Returns map
let returnsMap (rets: ReturnObs [] option): (DateTime * ReturnObs) [] option = 
    match rets with
    | None -> None
    | Some rets -> 
        let retsMap = rets |> Seq.map (fun xs -> xs.Date, xs) |> Seq.toArray
        Some (retsMap)

/// Use of partial application
let startSample, endSample =    
    myTranscripts
    |> Seq.map (fun xs -> xs.Date)
    |> fun dates -> (Seq.min dates).AddDays(-10.), (Seq.max dates).AddDays(10.)

let myReturns = tiingoReturns startSample endSample

/// SP500
let spyReturns = myReturns "SPY" |> returnsMap

/// Tesla
let tlseReturnsMap = myReturns "TSLA" |> returnsMap

/// Miscrosoft
let msftReturnsMap = myReturns "MSFT" |> returnsMap

(**
## Returns Around Earnings Announcements
*)

// Types
type ReturnsWindow = 
    {Transcript : Transcript
     ReturnsAroundEarnings : ReturnObs []}

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float}

let twoWeekReturnWindow (obs: Transcript) = 
    let twoWeekRets = tiingoReturns (obs.Date.AddDays(-7.0)) (obs.Date.AddDays(7.0))

    match twoWeekRets obs.Ticker with
    | None -> None
    | Some rets -> Some ({ Transcript = obs
                           ReturnsAroundEarnings = rets})

let postEarningsReturn (obs: ReturnsWindow): ReturnObs option =
    let eaDay = if obs.Transcript.Date.Hour < 16 then 
                    obs.Transcript.Date 
                else 
                    obs.Transcript.Date.Date.AddDays(1.0)

    obs.ReturnsAroundEarnings 
    |> Seq.tryFind (fun xs -> xs.Date.Date >= eaDay)

let ThreeDayWindow (obs: ReturnsWindow): ReturnObs [] option=
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

let ReturnsAroundEarningsAnnouncement (obs: ReturnsWindow option) = 
    match obs with 
    | None -> None
    | Some retWindow ->

        let retAroundEarnings = 
            retWindow 
            |> ThreeDayWindow

        Some {Transcript = retWindow.Transcript
              ReturnsAroundEarnings = retWindow.ReturnsAroundEarnings}

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

let getReturnsAroundAnnouncement (obs: Transcript) =
    obs
    |> TwoWeekReturnWindow
    |> ReturnsAroundEarningsAnnouncement
    |> AdjustedCumulativeReturns

myTranscripts |> Seq.length


(**
# Download and Export to Json
*)

(*
let t1950 = 
    myTranscripts
    |> Seq.take 1950
    |> Seq.choose getReturnsAroundAnnouncement
    |> Seq.toArray

let AnnouncementDayReturnToJson (transcripts: AnnouncementDayReturn [], fileName: string) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

AnnouncementDayReturnToJson (t1950, "data-cache/AnnouncementDay1950.json")
*)