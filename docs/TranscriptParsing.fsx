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

/// Match html node with "href" attribute and create transcript url
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

let exampleUrls = findTranscriptUrls exampleFrontPageDoc

/// First three urls
exampleUrls
|> Array.take 3
|> Array.iter (fun xs -> printfn$"{xs}")

(*** include-fsi-output***)

(**
## Transcript - Ticker & Exchange
*)

(** 
Apart from using the `CssSelect` method to search for transcript urls 
we can also use it to extract other key information like a company's 
ticker and exchange as well as the time and date of the earnings call.

Lets see if we can fetch Tesla's ticker and exchange from its 
[2021 Q2 earnings call](https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/): 

<img src="FsdocsImages\tesla_motley_fool.png" width="70%">
*) 

type TranscriptDocument = HtmlDocument
/// Tesla transcript html document
let teslaDoc: TranscriptDocument = HtmlDocument.Load "https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/"

teslaDoc.CssSelect("span[class='ticker']")
(***include-fsi-output***)

teslaDoc.CssSelect("span[class='ticker']")
|> List.map (fun x -> x.InnerText())
(***include-fsi-output***)

teslaDoc.CssSelect("span[class='ticker']")
|> List.map (fun x -> 
    x.InnerText()
     .Trim()
     .Replace("(","")
     .Replace(")",""))
|> List.distinct
|> List.tryExactlyOne     
(***include-fsi-output***)

// A function to do the same
let cleanTickerExchangeText (doc:TranscriptDocument) =
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> 
        x.InnerText()
         .Trim()
         .Replace("(","")
         .Replace(")",""))
    |> Seq.distinct
    |> Seq.tryExactlyOne

cleanTickerExchangeText teslaDoc
(***include-fsi-output***)

(**
Since we are not certain that we'll retrieve both a ticker and an exchange 
from *every* single transcript we parse, we can use match expressions and 
option types to make sure to return only those matches that contain both a 
valid ticker and exchange. 
*)

/// Match inner text from html node to a ticker and exchange
let tryTickerExchange (tickerInfo: string): option<string * string> =
    match tickerInfo.Split(":") with
    |[|exchange; ticker|] -> Some (ticker, exchange)
    | _ -> None

/// Search for ticker and exchange
let findTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc
    |> cleanTickerExchangeText
    |> Option.bind tryTickerExchange


// Tesla ticker and exchange
findTickerExchange teslaDoc

(*** include-it ***)

(**
## Transcript - Date & Time
*)

(**
Taking a closer look at Tesla's earnings transcript page, we can see that right 
below Tesla's ticker we spot the exact time and date of the earnings call.

Let's see if we can use `CssSelect` to fetch this information:
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

/// Search for transcript date
let findDate (doc: TranscriptDocument): option<string>=
    doc.CssSelect("span[id='date']")
    |> Seq.tryExactlyOne
    |> Option.bind cleanDate

/// Date of Tesla's call:
findDate teslaDoc

(*** include-it ***)

(**
### Time
*)

/// Format time string
let cleanTime (node: HtmlNode) =
    node.InnerText()
        .ToUpperInvariant()
        .Replace(".", "")
    |> fun txt ->    
        if (txt.Contains "ET")
        then txt.Replace("ET", "").Trim()
        else failwithf $"Expected ET timezone but got {txt}" 
   
/// Search for transcript time
let findTime (doc: TranscriptDocument) =
    doc.CssSelect("em[id='time']")
    |> Seq.tryExactlyOne
    |> Option.map cleanTime

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
        Some dt
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
    // Remove empty paragraphs
    |> Seq.filter (fun x -> x <> "")
    // Skip first 5 paragraphs
    |> Seq.skip 5
    |> Seq.toArray

let firstCharacters (paragraph:string) = 
    if paragraph.Length <= 50 
    then paragraph 
    else paragraph.[..49] + " ... "

// First two paragraphs
teslaDoc 
|> findParagraphs
|> Array.take 5
|> Array.map firstCharacters
|> Array.iteri (printfn "Paragraph %i: %s")

(*** include-output***)

(**
## Transcript Record
*)

(**
So far we have worked with individual functions that take in one single argument, 
an html transcript document. Since they all work with the `TranscriptDocument` 
type, we can easily combine these functions together to form one single function 
that returns all the individual bits of data that we want.

We'll use a record called `Transcript` to hold all the information we want.
*)

type TranscriptId =
    { Ticker: string 
      Exchange: string
      Date: System.DateTime }

type Transcript = 
    { TranscriptId : TranscriptId
      Paragraphs: string [] }

/// Search for ticker, exchange, date and paragraphs
let parseTrancriptDoc (doc: TranscriptDocument)=
    let matchExpr =  
        findTickerExchange doc, 
        findDateTime doc   
     
    match matchExpr with
    | Some (ticker, exchange), Some date -> 
        let transcriptId = {Ticker=ticker; Exchange=exchange; Date=date}
        Some { TranscriptId = transcriptId
               Paragraphs = findParagraphs doc }
    | _ -> None

/// Tesla transcript record
let teslaTranscript = parseTrancriptDoc teslaDoc

teslaTranscript 
|> Option.iter(fun x -> 
    printfn $"Id:\n{x.TranscriptId}\n"
    printfn $"First 5 paragraphs:"
    x.Paragraphs
    |> Array.truncate 5
    |> Array.map firstCharacters 
    |> Array.iter (printfn "%A"))
(*** include-output ***)

(**
Now that we have a working function that takes in a `TranscriptDocument` and
returns a `Transcript` type, lets try to parse all of the transcript urls from 
`exampleFrontPageDoc`.
*)

/// Parsing transcripts from front page
let exampleTranscripts = 
    exampleFrontPageDoc
    |> findTranscriptUrls 
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
    let tId = xs.TranscriptId
    printfn $"TranscriptId: %4s{tId.Ticker}, %6s{tId.Exchange}, {tId.Date}")

(*** include-output ***)

(**
## Data visualization with Plotly.NET
*)

(**
.NET has several useful libraries, including one dedicated for generating charts.
With [Plotly.NET](https://plotly.net/) you can create all sorts of charts from 
simple histograms all the way to 3D surface plots. Just like with FSharp Data, 
we can download Plotly.Net with .NET's package manager, Nuget.
*)

#r "nuget: Plotly.NET, 2.0.0-preview.6"
open Plotly.NET

/// Histogram
let transcriptTimesHistogram = 

    let callTimes = 
        exampleTranscripts
        |> Array.map (fun xs -> xs.TranscriptId.Date.TimeOfDay.ToString())
        |> Array.sort
    
    callTimes
    |> Chart.Histogram
    |> Chart.withTitle "Earnings calls by time of day (ET)"
    |> Chart.withY_AxisStyle "Count"
    |> Chart.withSize (750., 500.)

(*** do-not-eval ***)
transcriptTimesHistogram |> Chart.Show 
(*** hide ***)
transcriptTimesHistogram |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
Although we are working with a small sample, we can already notice that 
the time of the earnings calls are varied and that calls occur before 
market hours, during market hours and even after market hours.
*)

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

(***do-not-eval***)
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
