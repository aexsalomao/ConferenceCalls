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
    
- Before/During market hours: If the call had occurred at 11AM ET on a Tuesday, 
  we would fetch Monday's, Tuesday's, and Wednesday's closing prices.
    
- After market hours: If the call had occurred at 7PM ET on a Tuesday, 
  we would fetch Tuesday's, Wednesday’s, and Thursday's closing prices.
*)

(**
## Import packages and load scripts
*)

#load "TranscriptParsing.fsx"
#load "Common.fsx"

#r "nuget: Plotly.NET, 2.0.0-preview.6"

open TranscriptParsing
open Common

open System
open Newtonsoft.Json
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
### Reading Transcript data from .json file:
*)

/// JSON data reader
let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<Transcript>>(json)

/// Transcripts data
let myTranscripts = 
    readJson ("data-cache/TranscriptsDemo.json")
    |> Seq.take 100
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
    let checkReturns rets = 
        match rets with
        | [||] -> None
        | _ -> Some rets
        
    ticker
    |> Tiingo.request
    |> Tiingo.startOn tiingoStart
    |> Tiingo.endOn tiingoEnd
    |> Tiingo.getReturns
    |> checkReturns

(**
### Benchmark sample (SP500, "SPY")
*)

/// Sample range
let startSample, endSample =    
    myTranscripts
    |> Seq.map (fun xs -> xs.Date)
    |> fun dates -> (Seq.min dates).AddDays(-10.), (Seq.max dates).AddDays(10.)

/// SP500 (benchmark of choice)
let spyObs = 
    let getReturnsMap rets =
        rets
        |> Array.map (fun xs -> xs.Date, xs.Return)
        |> Map
    
    let checkSpy rets =
        match rets with 
        | None -> failwith "why isn't Tiingo working"
        | Some rets -> rets
        
    "SPY"
    |> tiingoReturns startSample endSample
    |> checkSpy
    |> getReturnsMap

(**
## Returns around earnings announcements
*)

(**
### Two-week return window
*)

(**
Sometimes a call might happen on a friday or right before or after a long holiday. 
In these particular case scenarios, we have to be extra careful when trying to find our three-day return window.

Because we don't have a database with all the non-trading days of a given year, 
instead of trying to match a three-day return window instantaneously, it is safer if we work from a range of return observations and 
try to find our three-day return window from there. A range of two full weeks of returns should in principle,
allow us to find the three-day window that we're looking for.
*)

type ReturnsWindow = 
    { Transcript: Transcript
      ReturnsAroundEarnings: ReturnObs [] }

/// Two weeks return window
let twoWeekReturnWindow (transcript: Transcript): ReturnsWindow option = 
    let minusOneWeek, plusOneWeek = 
        let flatDate = transcript.Date.Date
        flatDate.AddDays(-7.0), flatDate.AddDays(7.0)

    let retsAroundEarnings rets: ReturnsWindow = 
        { Transcript = transcript
          ReturnsAroundEarnings = rets }
          
    transcript.Ticker
    |> tiingoReturns minusOneWeek plusOneWeek
    |> Option.map retsAroundEarnings

(**
### First observed return after earnings call
*)

/// Find first observed return
let firstReturnAfterCall (obs: ReturnsWindow): ReturnObs option =
    let dateOfCall = 
        let date = obs.Transcript.Date
        if date.Hour < 16 then date.Date else date.Date.AddDays(1.0)
    
    obs.ReturnsAroundEarnings
    |> Seq.tryFind (fun xs -> 
        xs.Date.Date >= dateOfCall.Date)

(**
### Three-day window
*)

/// Three day return window
let findThreeDays (middleRet: ReturnObs) (rets: ReturnObs []) : ReturnObs [] option = 
   
    rets
    |> Seq.windowed 3
    |> Seq.tryFind (fun retWindow ->
        let middle = retWindow.[1]
        middle.Date.Date = middleRet.Date.Date)

(**
### Adjusted returns
*)

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float }

/// SPY returns window
let spyReturnsBetween (begWin:DateTime) (endWin:DateTime) =
    let rec loop (date:DateTime) rets =
        if date.Date <= endWin.Date then
            match Map.tryFind date spyObs with
            | Some spy -> loop (date.AddDays(1.0)) (spy::rets)
            | None -> loop (date.AddDays(1.0)) rets
        else rets
    loop begWin []
  
/// Computes adjusted return
let adjustedReturns (stock : ReturnObs []) : float = 
    let begWin = stock |> Seq.map(fun x -> x.Date) |> Seq.min
    let endWin = stock |> Seq.map(fun x -> x.Date) |> Seq.max
    
    let cumRet rets =
        (1.0, rets)
        ||> Seq.fold(fun acc ret -> acc*(1.0+ret)) 

    let spy = 
        spyReturnsBetween begWin endWin
        |> cumRet

    let stockRet = 
        stock 
        |> Seq.map(fun x -> x.Return) 
        |> cumRet                             
    stockRet - spy

/// Finds three-day window of adjusted returns and computes cumulative returns
let threeDayAdjustedReturns (transcript: Transcript): AnnouncementDayReturn option =
    let findRetWindows (stockObs: ReturnsWindow Option) =
        stockObs
        |> Option.map(fun stockObs ->
            match firstReturnAfterCall stockObs with
            | Some firstRet -> findThreeDays firstRet stockObs.ReturnsAroundEarnings
            | None -> None)
    
    let getAdjustedReturns input =
        match input with
        | Some stockRet ->
            let adjRet = adjustedReturns stockRet
            Some { Transcript = transcript
                   CumulativeReturn = adjRet }
        | None -> None

    transcript
    |> twoWeekReturnWindow
    |> findRetWindows
    |> Option.map getAdjustedReturns
    |> Option.flatten

(**
#### Compute cumualtive returns
*)

let myReturns = 
    myTranscripts
    |> Array.Parallel.choose threeDayAdjustedReturns

myReturns
|> Array.take 5
|> Array.iter (fun xs -> 
    printfn $"{xs.Transcript.Ticker} - Cum. Return: {Math.Round(xs.CumulativeReturn, 4)}")

(**
### Download and Export to Json
*)

let AnnouncementDayReturnToJson (fileName: string) (transcripts: AnnouncementDayReturn [])  = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

AnnouncementDayReturnToJson "data-cache/ReturnsAroundEarningsDemo.json" myReturns