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
*)

(**
Read data that was dowloaded using `TranscriptParsing.fsx`. 
*)

/// Reading JSON
let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)

/// Data
let myTranscripts = 
    readJson ("data-cache/Motley50.json")
    |> Seq.rev
    |> Seq.toArray

myTranscripts.Length

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
returns data for any given ticker. To be extra careful, we can check if Tiingo is returning any data for a given ticker with pattern matching.
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
    |> fun rets ->
    match rets with
    | [||] -> None
    | _ -> Some rets

/// Returns map
let returnsMap (rets: ReturnObs [] option) = 
    match rets with
    | Some rets ->
        let retsMap = 
            rets 
            |> Seq.map (fun xs -> xs.Date, xs) 
            |> Map.ofSeq
        Some (retsMap)
    | None -> None

(**
### Use of partial application
*)

(**
From the collected transcripts, we can set a date range from which we can collect returns. The `tiingoReturns` function allows for the user to set dates ...
*)

/// Find sample range
let startSample, endSample =    
    myTranscripts
    |> Seq.map (fun xs -> xs.Date)
    |> fun dates -> (Seq.min dates).AddDays(-10.), (Seq.max dates).AddDays(10.)

/// "Bake-in dates"
let samplePeriodReturns = tiingoReturns startSample endSample

/// SP500
let spyObs = samplePeriodReturns "SPY"

(**
## Returns Around Earnings Announcements
*)

(**
### Two-week return window
*)

(**
Since we are not working with an external database that contains all trading calendar days ...
*)

type ReturnsWindow = 
    { Transcript : Transcript
      ReturnsAroundEarnings : ReturnObs [] }

/// Two weeks return window
let twoWeekReturnWindow (transcript: Transcript): ReturnsWindow option = 
    let minusOneWeek, plusOneWeek = 
        transcript.Date 
        |> fun date -> date.AddDays(-7.0), date.AddDays(7.0)
    
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
    |> Seq.tryFind (fun xs -> xs.Date.Day >= dateOfCall.Day)

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
                   | [|d1; d2; d3|] when d2.Day = middleRet.Date.Day -> Some (retWindow)
                   | _ -> None)
    |> Seq.tryExactlyOne

(**
### Adjusted returns
*)

let threeDayAdjustedReturns (transcript: Transcript): ReturnsWindow option =

    /// Check if Tiingo is returning data
    match spyObs, 
          twoWeekReturnWindow transcript with
    | Some spyObs, 
      Some stockObs ->

      /// Find first observed return after call
        match firstReturnAfterCall stockObs with
        | Some firstRet ->

        /// Find three-day window
            match  findThreeDays spyObs firstRet, 
                   findThreeDays stockObs.ReturnsAroundEarnings firstRet with              
            | Some spyWindow, 
              Some stockWindow ->

            /// Compute excess returns from SPY
                let adjustedReturns = 
             
                    let spyWindowMap = 
                        spyWindow 
                        |> Array.map (fun xs -> xs.Date, xs) 
                        |> Map.ofArray

                    stockWindow
                    |> Array.choose 
                        (fun stockRet ->
                         match Map.tryFind stockRet.Date spyWindowMap with
                         | Some spyRet -> 
                            let adjRet = stockRet.Return - spyRet.Return 
                            Some { Symbol = stockRet.Symbol
                                   Date = stockRet.Date
                                   Return = adjRet}
                         | None -> None)
                Some { Transcript = transcript
                       ReturnsAroundEarnings = adjustedReturns }
            | _ -> None
        |_ -> None   
    | _ -> None

(**
### Cumulative returns
*)

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float }

let cumulativeReturns (obs: ReturnsWindow option): AnnouncementDayReturn option = 
    match obs with
    | Some obs ->     
        (1.0, obs.ReturnsAroundEarnings)
        ||> Array.fold (fun acc x -> acc*(1.0+ x.Return))
        |> fun xs -> 
            let cumRet = xs - 1.
            Some { Transcript = obs.Transcript
                   CumulativeReturn = cumRet}
    | None -> None

let getReturnsAroundAnnouncement (obs: Transcript): AnnouncementDayReturn option =
    obs
    |> threeDayAdjustedReturns
    |> cumulativeReturns

let t100 = 
    myTranscripts
    |> Seq.take 100
    |> Seq.choose getReturnsAroundAnnouncement
    |> Seq.toArray

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