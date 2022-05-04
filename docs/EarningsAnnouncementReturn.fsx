(**
---
title: Earnings Announcement Return (EAR)
category: Scripts
categoryindex: 2
index: 2
---

[![Script](img/badge-script.svg)]({{root}}/{{fsdocs-source-basename}}.fsx)&emsp;
[![Notebook](img/badge-notebook.svg)]({{root}}/{{fsdocs-source-basename}}.ipynb)
*)

(**
# Earnings Announcement Return (EAR)
*)

(**
In the `TranscriptParsing.fsx` script, we downloaded earnings call transcripts, 
ticker and exchange information, and even the exact date and time of each earnings call. 
Ok great, now what ?
*)

(**
In finance, a growing body of literature is concerned with applying 
state of the art *text-mining* techniques on textual data with the objective 
of conducting *sentiment analysis*. Such analysis is often conducted using 
statistical learning methods such as *supervised* and *unsupervised* learning. 
The key difference between these sets of methods/algorithms lies within their purpose. 
While supervised learning is used for solving the task of *prediction*, 
unsupervised learning is used for other tasks such as *data inference*. 
Additionally, as the names suggest, while supervised learning algorithms 
*learn* by working with *labeled datasets*, unsupervised learning 
algorithms do not. For this very reason, it is often the case that, when compared 
to unsupervised learning, supervised learning techniques are regarded as 
less complex and more "trustworthy".
*)

(**
Here are some examples:

- Supervised learning: Support vector machine, Neural network, Linear and logistics regression, 
random forest, and Classification trees.

- Unsupervised learning: K-means, Hierarchical clustering, Principal Component Analysis (PCA)
*)

(**
In the case of the earnings calls dataset that we formed by parsing motley fool, 
we can *label* each call according to the realized returns around the time of the 
earnings call. We can then use these returns as a proxy that indicates the overall 
"sentiment" of each call. The literature refers to these returns as the Earnings Announcement 
Return, or EAR. From EAR, we can proceed to define or label each earnings calls as 
being either "Positive" or "Negative". The EAR any given firm (stock) is simply its 
abnormal return over a three day window ***centered*** on the earnings announcement.
*)

(**
## Import packages and load scripts
*)

open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#load "Types.fsx"
#load "Common.fsx"
#r "nuget: FSharp.Data"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

if false then
    let tiingoKey = System.Environment.GetEnvironmentVariable "TIINGO_API_KEY"
    ()

open Types
open Common
open Common.Tiingo

open Newtonsoft.Json
open Plotly.NET

fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
### Reading Transcript data from .json file:
*)

/// JSON data reader
let readEarningsCallJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<array<EarningsCall>>(json)

/// Calls data
let myCalls = 
    [|
    "data-cache/EarningsCall2018.json"
    "data-cache/EarningsCall2019.json"
    "data-cache/EarningsCall2020.json"
    "data-cache/EarningsCall2021.json"
    |]
    |> Array.collect readEarningsCallJson
    |> Array.sortBy (fun xs -> xs.CallId.Date)

(**
## Barplots: Timing of calls
*)

let callsByTimeOfDay (calls : EarningsCall []) = 
    calls
    |> Array.countBy (fun xs -> xs.CallId.Date.Hour)
    |> Array.sortBy (fun (hour, _) -> hour)
    |> Chart.Column
    |> Chart.withTitle $"Earnings Calls by time of day (N: {Seq.length calls})"
    |> Chart.withX_AxisStyle "Hour"
    |> Chart.withY_AxisStyle "Count"

(*** do-not-eval ***)
myCalls |> callsByTimeOfDay |> Chart.Show
(*** hide ***)
myCalls |> callsByTimeOfDay |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Tiingo returns
*)

(**
Provided that Tiingo supports the ticker we are looking for, we can use 
the `Tiingo` module from `Common.fsx` to download ticker related data such
as closing price and volume. Since we might be interested in analyzing the 
stock's movement following the earnings call we'll fetch data up until 60 days 
after each call. We can use this same 60 day window of ticker observations to
compute the EAR for each call.
*)

/// Tiingo data
let tiingoWindow (tiingoStart: DateTime)
                 (tiingoEnd: DateTime)
                 (ticker: string) =
    let checkObs obs = 
        match obs with
        | [||] -> None
        | _ -> Some obs
        
    ticker
    |> Tiingo.request
    |> Tiingo.startOn tiingoStart
    |> Tiingo.endOn tiingoEnd
    |> Tiingo.get
    |> checkObs

let calcReturn pv fv = 
    (fv / pv) - 1.0

let getReturnObs (ticker: string) (obs: TiingoObs []) = 
    obs
    |> Seq.pairwise
    |> Seq.map (fun (yesterday, today) -> 
        { Symbol = ticker
          Date = today.Date
          Return = calcReturn (float yesterday.AdjClose) (float today.AdjClose) })
    |> Seq.toArray

let earBarPlot (ticker: string) = 
    myCalls 
    // Find first matching observation
    |> Seq.tryFind (fun xs -> xs.CallId.Ticker = ticker) 
    |> Option.bind (fun call -> 
        tiingoWindow (call.CallId.Date.AddDays(-7.)) (call.CallId.Date.AddDays(7.)) ticker
        |> fun xs ->
            match xs with
            | Some obs -> 
                getReturnObs ticker obs
                // Plot
                |> Array.map (fun xs -> xs.Date, xs.Return)
                |> Chart.Bar
                |> Chart.withTitle 
                    $"{ticker} Earnings Call {call.CallId.Date} Q{call.CallId.FiscalQuarter.ToString()}"
                |> Some
            | None -> None)

let msftEarPlot = earBarPlot "MSFT"

(*** do-not-eval ***)
msftEarPlot |> Option.map Chart.Show
(*** hide ***)
msftEarPlot |> Option.map GenericChart.toChartHTML
(*** include-it-raw ***)

(**
### Earnings Announcement Return
*)

/// Sample range
let startSample, endSample =    
    myCalls
    |> Seq.map (fun xs -> xs.CallId.Date)
    |> fun dates -> 
        (Seq.min dates).AddDays(-10.), (Seq.max dates).AddDays(10.)

/// SP500 (SPY)
let spyObs = 
    let spyTicker = "SPY"

    let getReturnsMap (tiingoObs: Tiingo.TiingoObs []) =
        getReturnObs spyTicker tiingoObs
        |> Array.map (fun xs -> xs.Date, xs)
        |> Map

    let checkSpy rets =
        match rets with 
        | None -> failwith "why isn't Tiingo working"
        | Some rets -> rets
        
    spyTicker
    |> tiingoWindow startSample endSample
    |> checkSpy
    |> getReturnsMap

(**
## Earnings Announcement Return
*)

(**
### Tiingo observation window
*)

(**
Sometimes a call might happen on a friday or right before or after a long holiday. 
In these particular case scenarios, we have to be extra careful when trying to find 
our three-day return window.

Because we don't have a database with all the non-trading days of a given year, 
instead of trying to match a three-day return window instantaneously, it is safer 
if we work from a range of return observations and try to find our three-day return 
window from there. 
*)

(**
#### Three-day windows
*)

/// Three day return window
let findThreeDays (middleObs: ReturnObs) (rets: ReturnObs []): ReturnObs [] option = 
    rets
    |> Seq.windowed 3
    |> Seq.tryFind (fun retWindow ->
        let middle = retWindow.[1]
        middle.Date.Date = middleObs.Date.Date)

/// SPY returns window
let spyReturnsBetween (begWin: DateTime) (endWin: DateTime) =
    let rec loop (date: DateTime) rets =
        if date.Date <= endWin.Date then
            match Map.tryFind date spyObs with
            | Some spy -> loop (date.AddDays(1.0)) (spy::rets)
            | None -> loop (date.AddDays(1.0)) rets
        else rets
    loop begWin []

(**
#### Adjusted returns
*)

/// Abnormal returns from three day window
let computeAdjReturns (stock : ReturnObs []) = 
    let begWin, endWin = 
        stock
        |> Seq.sortBy (fun xs -> xs.Date)
        |> Seq.map (fun xs -> xs.Date)
        |> fun xs -> 
            (xs |> Seq.head), (xs |> Seq.last)
    
    let cumRet rets =
        (1.0, rets)
        ||> Seq.fold(fun acc ret -> acc*(1.0+ret)) 

    let spy = 
        spyReturnsBetween begWin endWin
        |> Seq.map (fun xs -> xs.Return)
        |> cumRet
        
    let stockRet = 
        stock 
        |> Seq.map(fun x -> x.Return) 
        |> cumRet

    stockRet - spy

(**
#### EAR
*)

type Sentiment = 
    | Positive
    | Negative
    | Neutral

type EarningsAnnouncementReturn =
    { EarningsCall: EarningsCall
      TiingoObs: Tiingo.TiingoObs []
      Sentiment: Sentiment option 
      Ear: float option }

/// Find first observed return
let firstReturnAfterCall (call: EarningsCall) (returnObs: ReturnObs []) = 
    let date = call.CallId.Date
    if date.Hour < 16 then date.Date 
    else date.Date.AddDays(1.0)
    
    |> fun dateOfCall -> 
        returnObs
        |> Seq.tryFind (fun xs -> xs.Date.Date >= dateOfCall.Date)

let computeEar (call: EarningsCall) (tiingoObs: Tiingo.TiingoObs []) = 

    let getAdjReturns middleObs returnObs = 
        match findThreeDays middleObs returnObs with
        | Some threeDayWindow -> Some (computeAdjReturns threeDayWindow)
        | None -> None
    
    getReturnObs call.CallId.Ticker tiingoObs
    |> fun retObs ->
        firstReturnAfterCall call retObs
        |> Option.bind (fun middleObs -> 
            getAdjReturns middleObs retObs)

let generateEar (call: EarningsCall) = 
    let tiingoWindow = 
        let flatDate = call.CallId.Date.Date
        tiingoWindow (flatDate.AddDays(-10.0)) (flatDate.AddDays(70.0)) call.CallId.Ticker

    match tiingoWindow with
    | Some tiingoObs -> 
        // For now lets set Sentiment to None
        Some { EarningsCall = call
               TiingoObs = tiingoObs
               Sentiment = None
               Ear = computeEar call tiingoObs}
    | None -> None

let tslaCall = 
    myCalls
    |> Array.tryFind (fun xs -> xs.CallId.Ticker = "TSLA")
    |> Option.bind generateEar

tslaCall |> Option.bind (fun xs -> xs.Ear)

(**
### Async methods
*)

module Async =
    let ParallelThrottled xs = Async.Parallel(xs, 100)

let asyncCall (call: EarningsCall) =
    let rec loop attempt n =
        async {
            try
                return generateEar call
            with e ->
                if attempt > 0 then
                    do! Async.Sleep 2000 // Wait 2 seconds in case we're throttled.
                    return! loop (attempt - 1) n
                else return! failwithf "Failed to request '%s'. Error: %O" call.CallId.Ticker e }
    loop 10 call 

let asyncCalls (calls: EarningsCall []) = 
    calls
    |> Seq.map asyncCall
    |> Async.ParallelThrottled
    |> Async.RunSynchronously
    |> Array.choose id

(***do-not-eval***)
let getEarsByYear year = 
    myCalls
    |> Array.filter (fun xs -> xs.CallId.Date.Year = year)
    |> asyncCalls

let ears2018 = getEarsByYear 2018
let ears2019 = getEarsByYear 2019
let ears2020 = getEarsByYear 2020
let ears2021 = getEarsByYear 2021

let calls2018, calls2019, calls2020, calls2021 = 
    myCalls
    |> fun xs -> 
        (xs |> Array.filter (fun xs -> xs.CallId.Date.Year = 2018)),
        (xs |> Array.filter (fun xs -> xs.CallId.Date.Year = 2019)),
        (xs |> Array.filter (fun xs -> xs.CallId.Date.Year = 2020)),
        (xs |> Array.filter (fun xs -> xs.CallId.Date.Year = 2021))

calls2018.Length
ears2018.Length

calls2019.Length
ears2019.Length

calls2020.Length
ears2020.Length

calls2021.Length
ears2021.Length

(**
### Download and Export to Json
*)

(***do-not-eval***)
let earToJson (fileName: string) (ears: EarningsAnnouncementReturn [])  = 
    JsonConvert.SerializeObject(ears)
    |> fun json -> IO.File.WriteAllText(fileName, json)

earToJson "data-cache/EarningsAnnouncementReturn2018.json" ears2018
earToJson "data-cache/EarningsAnnouncementReturn2019.json" ears2019
earToJson "data-cache/EarningsAnnouncementReturn2020.json" ears2020
earToJson "data-cache/EarningsAnnouncementReturn2021.json" ears2021