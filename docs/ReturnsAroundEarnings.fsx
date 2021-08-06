(**
---
title: Returns around earnings announcements
category: Scripts
categoryindex: 2
index: 2
---
*)

(**
# Returns around earnings announcements
*)

(**
In the `TranscriptParsing.fsx` script, we downloaded earnings call transcripts, ticker and exchange information, 
and even the exact date and time of each earnings call. With this information we can proceed to computing stock returns 
around the time of the call. 

To start, we first need to fetch end-of-day or closing price data from Tiingo’s API. 
To be more precise, we will be fetching closing prices from a three-day window. Why a three-day window?

While calls that occur before or during market hours are expected to affect stock prices on that very same day, 
calls that occur after market hours are expected to only impact prices on the following day. 
For any given earnings call, what we want is to compute returns from a window of 3 closing prices: 
the last closing price prior to the call and the first two observed closing-prices after the call took place. 

For example:
- Before/During market hours: If the call had occurred at 11AM ET on a Tuesday, we would fetch Monday's, Tuesday's, and Wednesday's closing prices.
- After market hours: If the call had occurred at 7PM ET on a Tuesday, we would fetch Tuesday's, Wednesday’s, and Thursday's closing prices.
*)

(**
## Import packages and load scripts
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
### Reading Transcript data from .json file:
*)

/// JSON data reader
let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)

/// Data
let myTranscripts = 
    readJson ("data-cache/TranscriptsDemo.json")
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

(**
## Tiingo returns
*)

(**
Provided that Tiingo supports the ticker we are looking for, we can use the `Tiingo` module from `Common.fsx` to download 
returns data for any given ticker. To be extra careful, we can check if Tiingo is returning any data at all with some pattern matching.
*)

/// Tiingo returns
let tiingoReturns (tiingoStart: DateTime)
                  (tiingoEnd: DateTime)
                  (ticker: string): ReturnObs [] option =
    ticker
    |> Tiingo.request
    |> Tiingo.startOn tiingoStart
    |> Tiingo.endOn tiingoEnd
    |> Tiingo.getReturns
    |> fun rets ->
    match rets with
    | [||] -> None
    | _ -> Some rets

(**
### Use of partial application
*)

(**
From the collected transcripts, we can set a date range from which we can collect returns.
*)

/// Sample range
let startSample, endSample =    
    myTranscripts
    |> Seq.map (fun xs -> xs.Date)
    |> fun dates -> (Seq.min dates).AddDays(-10.), (Seq.max dates).AddDays(10.)

/// Tiingo returns function with "baked-in dates"
let samplePeriodReturns = tiingoReturns startSample endSample

/// SP500 (benchmark of choice)
let spyObs = samplePeriodReturns "SPY"

(**
## Returns around earnings announcements
*)

(**
### Two-week return window
*)

(**
Sometimes a call might happen on a friday or right before or after a long holiday. 
In these particular case scenarios, we have to be extra careful when trying to find our three-day return window.

Because we don't have a database with all the non-trading days of any given year, 
instead of trying to match a three-day return window in one go, it is safer if we work from a range of dates and 
try to find our three-day return window from there. A range of two full weeks should in principle secure us a three-day return window.
*)

type ReturnsWindow = 
    { Transcript: Transcript
      ReturnsAroundEarnings: ReturnObs [] }

/// Two weeks return window
let twoWeekReturnWindow (transcript: Transcript): ReturnsWindow option = 
    let minusOneWeek, plusOneWeek = 
        transcript.Date 
        |> fun date -> date.AddDays(-7.0), date.AddDays(7.0)
    
    /// Use of partial aplication
    let twoWeekTiingoRets = tiingoReturns minusOneWeek plusOneWeek

    match twoWeekTiingoRets transcript.Ticker with
    | None -> None
    | Some rets -> Some { Transcript = transcript
                          ReturnsAroundEarnings = rets }

(**
### First observed return after earnings call
*)

/// Find first observed return
let firstReturnAfterCall (obs: ReturnsWindow): ReturnObs option =
     
    let dateOfCall = 
        obs.Transcript.Date
        |> fun date -> if date.Hour < 16 then date else date.AddDays(1.0)
    
    obs.ReturnsAroundEarnings
    |> Seq.tryFind (fun xs -> xs.Date.Date >= dateOfCall.Date.Date)

(**
### Three-day window
*)

let findThreeDays (rets: ReturnObs []) 
                  (middleRet: ReturnObs): ReturnObs [] option = 
    rets
    |> Seq.windowed 3
    |> Seq.choose (fun retWindow ->
                   let dates = retWindow |> Array.map (fun rets -> rets.Date)
                   match dates with
                   | [|d1; d2; d3|] when d2.Date.Date = middleRet.Date.Date -> Some (retWindow)
                   | _ -> None)
    |> Seq.tryExactlyOne

(**
### Adjusted returns
*)

let adjustedReturns (stock : ReturnObs []) (benchmark: ReturnObs []): ReturnObs [] = 
                        
    let benchMap = 
        benchmark 
        |> Array.map (fun xs -> xs.Date, xs) 
        |> Map.ofArray
    
    stock
    |> Array.choose (fun stockRet ->
                     match Map.tryFind stockRet.Date benchMap with
                     | Some spyRet -> 
                        let adjRet = stockRet.Return - spyRet.Return 
                        Some { Symbol = stockRet.Symbol
                               Date = stockRet.Date
                               Return = adjRet}
                     | None -> None)

let threeDayAdjustedReturns (transcript: Transcript): ReturnsWindow option =

    /// Check if Tiingo is returning data
    match spyObs, twoWeekReturnWindow transcript with
    | Some spyObs, Some stockObs ->

        /// Find first observed return after call
        match firstReturnAfterCall stockObs with
        | Some firstRet ->

            /// Find three-day window
            match  findThreeDays spyObs firstRet, findThreeDays stockObs.ReturnsAroundEarnings firstRet with              
            | Some spyWindow, Some stockWindow ->

                /// Compute excess returns from SPY
                let adjRet = adjustedReturns stockWindow spyWindow
                Some { Transcript = transcript
                       ReturnsAroundEarnings = adjRet }
            | _ -> None
        |_ -> None   
    | _ -> None

(**
### Cumulative returns
*)

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float }

let cumulativeReturns (obs: ReturnsWindow): AnnouncementDayReturn = 
    (1.0, obs.ReturnsAroundEarnings)
    ||> Array.fold (fun acc x -> acc*(1.0+ x.Return))
    |> fun xs -> 
       let cumRet = xs - 1.
       { Transcript = obs.Transcript
         CumulativeReturn = cumRet }

(**
### Download and Export to Json
*)

let AnnouncementDayReturnToJson (fileName: string) (transcripts: AnnouncementDayReturn [])  = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

let myReturns = 
    myTranscripts
    |> Array.take 100
    |> Array.Parallel.choose threeDayAdjustedReturns
    |> Array.Parallel.map cumulativeReturns

AnnouncementDayReturnToJson "data-cache/AnnouncementDayReturnsDemo.json" myReturns