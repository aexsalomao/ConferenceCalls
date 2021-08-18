(**
---
title: Parsing Motley Fool
category: Scripts
categoryindex: 2
index: 1
---
*)

(**
# Transcript Parsing
*)

(**
The objective of this `TranscriptParsing.fsx` script is to give a few examples
on how to parse html documents with F#. More specifically, we will be attempting
to parse earnings call transcripts from [Motley Fool](https://www.fool.com).

Before getting started, lets download the [FSharp.Data](https://fsprojects.github.io/FSharp.Data/)
nuget package using .NET's package manager [NuGet](https://www.nuget.org/packages/FSharp.Data/):
*)

#r "nuget: FSharp.Data"

open System
open FSharp.Data

(**
## Transcript - Url
*)

(**
We can download or parse individual html documents with their url.
Since each call transcript will have a different url, we need
to find an effective and consistent way to fetch individual urls 
from motley fool's website. Fortunately, if we take a look at <a href="https://www.fool.com/earnings-call-transcripts/?page=1" target="_blank">motley fool's front page</a>, we see that all call transcripts are tagged with hyperlinks. 
*)

(**
<img src="FsdocsImages\motley_fool_front_page.png" width="70%" >
*)

(**
Since the transcripts are tagged with a specific hypertext reference 
(href) (`"/earnings/call-transcripts"`), we can use the `CssSelect` 
method from FSharp Data to find all elements in a given front page 
that match the transcript href that we are looking for. After fetching 
the urls, we can download any transcript we want as an html document 
using the `HtmlDocument.Load` method, also from FSharp Data.
*)

type FrontPageDocument = HtmlDocument

/// Match html node with "href" attribute
let tryHref (node: HtmlNode): string option =
    let foolLink (attrib:HtmlAttribute) = $"https://www.fool.com{attrib.Value()}"
    node.TryGetAttribute("href") |> Option.map foolLink

let makeFoolUrl (attrib:HtmlAttribute) = 
    match attrib.Name(), attrib.Value() with
    | "href", stub -> $"https://www.fool.com{stub}"
    | _, _ -> failwithf $"Expected href attribute but got {attrib}"

/// Search for transcript urls
let findTranscriptUrls (pageDoc: FrontPageDocument): string [] =  
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose (HtmlNode.tryGetAttribute "href")
    |> Seq.map makeFoolUrl
    |> Seq.toArray
    
(** 
Lets take a look at the first three call transcript urls `CssSelect` was able to match:
*)

let exampleFrontPageDoc: FrontPageDocument = HtmlDocument.Load "https://www.fool.com/earnings-call-transcripts/?page=1"

/// First three urls
exampleFrontPageDoc
|> findTranscriptUrls
|> Array.take 3
|> Array.iter (printfn "%s")

(*** include-output***)

(**
## Transcript - Ticker & Exchange
*)

(**
Apart from using the `CssSelect` method to search for transcript urls 
we can also use it to extract other key information like a company's 
ticker and exchange as well as the time and date of the earnings call. 
Since we are not certain that we'll retrieve both a ticker and an exchange 
from *every* single transcript we parse, we can use match expressions and 
option types to make sure to return only those matches that contain both a 
valid ticker and exchange. 
*)

type TranscriptDocument = HtmlDocument

/// Match inner text from html node to a ticker and exchange
let tryTickerExchange (tickerInfo: string): option<string * string> =
    tickerInfo.Split(":")
    |> fun xs -> 
        match xs with
        |[|exchange; ticker|] -> Some (ticker, exchange)
        | _ -> None

/// Search for ticker and exchange
let findTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    // Filtering unwanted strings
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> Option.bind tryTickerExchange

(**
Lets see if we can fetch Tesla's ticker and exchange from its 
<a href="https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/" target="_blank">latest earnings call</a>:
*)

(**
<img src="FsdocsImages\tesla_motley_fool.png" width="70%">
*) 

/// Tesla transcript html document
let teslaDoc: TranscriptDocument = HtmlDocument.Load "https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/"

// Tesla ticker and exchange
findTickerExchange teslaDoc

(*** include-it ***)

(**
## Transcript - Date & Time
*)

(**
We can use the `CssSelect` method to search for the exact date and time of the earnings call.
*)

(**
### Date
*)

/// Format date string
let cleanDate (node: HtmlNode): option<string> = 
    node.InnerText()
        .ToUpperInvariant()
        .Replace(".", "")
        .Replace(",", "")
        .Trim()
        .Split(" ")
    |> fun dateArr ->     
        match dateArr with
        |[|month; day; year|] -> Some ($"{month.[..2]} {day} {year}") 
        | _ -> None

/// Match html node with some date
let tryDate (node: HtmlNode option): option<string> = node |> Option.bind cleanDate

/// Search for transcript date
let findDate (doc: TranscriptDocument): option<string>=
    doc.CssSelect("span[id='date']")
    |> Seq.tryExactlyOne
    |> tryDate

/// Date of Tesla's call:
findDate teslaDoc

(*** include-it ***)

(**
### Time
*)

/// Format time string
let cleanTime (node: HtmlNode ) = 
    let txt = node.InnerText().ToUpperInvariant().Replace(".", "")
    if (txt.Contains "ET")
    then txt.Replace("ET", "").Trim()
    else failwithf $"Expected ET timezone but got {txt}"

/// Match html node with some time
let tryTime (node: HtmlNode option): option<string> =
    match node with
    | None -> None
    | Some timeNode -> Some (cleanTime timeNode)

/// Search for transcript time
let findTime (doc: TranscriptDocument) =
    doc.CssSelect("em[id='time']")
    |> Seq.tryExactlyOne
    |> tryTime

/// Time of Tesla's call
findTime teslaDoc

(*** include-it ***)

(**
### DateTime
*)

(**
Now that we have working functions for both the date and time of each call, 
lets combine these functions together and convert the information we have on
the date and time of an earnings call to a <a href="https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0" target="_blank">DateTime struct</a> :
*)

/// DateTime converter
let convertToDateTime (date: string, time: string): DateTime =
    let dateExpr = $"{date} {time}"
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.InvariantCulture)

/// Search for and match date and time
let findDateTime (doc: TranscriptDocument): option<DateTime> =
    match findDate doc, findTime doc with
    | Some date, Some time -> 
        let dt = convertToDateTime (date, time)
        Some (dt) 
    | _ -> None

/// Tesla call DateTime
findDateTime teslaDoc

(*** include-it ***)

(**
## Transcript - Paragraphs
*)

(**
The transcript itself can also be easily parsed using the `CssSelect()` method.
In html, blocks of text or paragraphs are defined with the <p> tag: 
*)

let findParagraphs (doc: HtmlDocument): string [] = 
    doc.CssSelect("p")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> x <> "")
    // Skip first 5 paragraphs
    |> Seq.skip 5
    |> Seq.toArray

// First two paragraphs
teslaDoc 
|> findParagraphs
|> Array.take 2
|> Array.iter (printfn "%s")

(*** include-output***)

(**
## Transcript Record
*)

(**
So far we have worked with individual functions that take in one single argument
, an html transcript document. Since they all work with the `TranscriptDocument` 
type, we can easily combine these functions together to form one single function 
that returns all the individual bits of data that we want.

We'll use a record called `Transcript` to hold all the information we want.
*)

type TranscriptId = 
    | Indexed of ticker:string * exchange:string * date: DateTime

type Transcript = 
    { TranscriptId : TranscriptId
      Paragraphs: string [] }

/// Search for ticker, exchange, date and paragraphs
let parseTrancriptDoc (doc: TranscriptDocument)=
    let matchExpr =  
        findTickerExchange doc, 
        findDateTime doc, 
        findParagraphs doc    
     
    match matchExpr with
    | Some (ticker, exchange), Some date, paragraphs -> 
        let transcriptId = Indexed (ticker, exchange, date)
        Some { TranscriptId = transcriptId
               Paragraphs = paragraphs }
    | _ -> None

/// Tesla transcript record
let teslaTranscript = parseTrancriptDoc teslaDoc

(*** include-output ***)

(**
Now that we have a working function that takes in a `TranscriptDocument` and
returns a `Transcript` type, lets try to parse all of the transcript urls from 
`exampleFrontPageDoc`.
*)

/// Parsing transcripts from front page
let exampleTranscripts = 
    findTranscriptUrls exampleFrontPageDoc
    |> Array.choose (fun tUrl -> 
        let doc = HtmlDocument.Load tUrl
        parseTrancriptDoc doc)

/// Total number of transcripts
printfn $"N: {exampleTranscripts.Length}"

(*** include-output ***)

/// First 5 transcripts
exampleTranscripts
|> Array.take 5
|> Array.iter (fun xs -> 
    match xs.TranscriptId with 
    | Indexed (ticker, exchange, date) -> 
        printfn $"TranscriptId: ({ticker}, {exchange}, {date})")

(*** include-output ***)


(**
## Data visualization with Plotly.NET
*)

(**
.NET has several useful libraries, including one dedicated for generating charts.
With Plotly.NET[https://plotly.net/] you can create all sorts of charts from 
simple histograms all the way to 3D surface plots. Just like with FSharp Data, 
we download Plotly.Net with .NET's package manager, Nuget.

Lets investigate the distribution of the time of day of the transcripts we just parsed: 
*)

#r "nuget: Plotly.NET, 2.0.0-preview.6"
open Plotly.NET

/// Histogram
let transcriptTimesHistogram = 

    let description =
        let text = $"Although we are working with a small sample of {exampleTranscripts.Length} 
                     observations, we can already notice that the time of the earnings calls are 
                     varied and that calls occur before market hours, during market hours and 
                     even after market hours."

        ChartDescription.create "Comments" text

    let callTimes = 
        exampleTranscripts
        |> Array.map (fun xs -> 
            match xs.TranscriptId with
            | Indexed (_, _, date) -> date.TimeOfDay.ToString())
        |> Array.sort
    
    callTimes
    |> Array.sort
    |> Chart.Histogram
    |> Chart.withTitle "Earnings calls by time of day (ET)"
    |> Chart.withY_AxisStyle "Count"
    |> Chart.WithDescription description
    |> Chart.withSize (750., 500.)

(*** do-not-eval ***)
transcriptTimesHistogram |> Chart.Show 
(*** hide ***)
transcriptTimesHistogram |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Async methods
*)

let asyncTranscript (url: string) = 
    let rec loop attempt url =
        async {
            try 
                let! transcriptDoc = HtmlDocument.AsyncLoad url
                let transcriptRec = parseTrancriptDoc transcriptDoc
                return transcriptRec
            with e ->
                if attempt > 0 then
                    do! Async.Sleep 2000 // Wait 2 seconds in case we're throttled.
                    return! loop (attempt - 1) url
                else return! failwithf "Failed to request '%s'. Error: %O" url e }
    loop 5 url

let asyncPage (n: int) =
    let rec loop attempt n =
        async {
            printfn $"{n}"
            let frontPageP = $"https://www.fool.com/earnings-call-transcripts/?page={n}"
            try 
                let! pageDoc = HtmlDocument.AsyncLoad frontPageP 
                return findTranscriptUrls pageDoc
            with e ->
                if attempt > 0 then
                    do! Async.Sleep 2000 // Wait 2 seconds in case we're throttled.
                    return! loop (attempt - 1) n
                else return! failwithf "Failed to request '%s'. Error: %O" frontPageP e }
    loop 5 n 
    
(**
### Parse Transcript Pages
*)

module Async =
    let ParallelThrottled xs = Async.Parallel(xs, 5)

let asyncPages (pages: int list) = 
    let urls = 
        pages 
        |> Seq.map asyncPage
        |> Async.ParallelThrottled 
        |> Async.RunSynchronously
        |> Array.collect id
    let transcripts =
        urls
        |> Array.map asyncTranscript
        |> Async.ParallelThrottled
        |> Async.RunSynchronously
        |> Array.choose id
    transcripts

let exampleTranscriptsAsync = asyncPages [ 200 .. 201 ]

(**
## Export to json
*)

#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let transcriptsToJson (fileName: string) (transcripts: Transcript []) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(*** do-not-eval***)
transcriptsToJson "data-cache/TranscriptsDemo.json" exampleTranscripts