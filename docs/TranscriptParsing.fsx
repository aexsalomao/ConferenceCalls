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
#r "nuget: Newtonsoft.Json"

open FSharp.Data
open Newtonsoft.Json
open System

(**
## Transcript - Url
We can download or parse individual html documents with their url.
Since each call transcript will have a different url, we need
to find an effective and consistent way to fetch individual urls 
from motley fool's website. Fortunately, if we take a look at <a href="https://www.fool.com/earnings-call-transcripts/?page=1" target="_blank">motley fool's front page</a>, we see that all call transcripts are tagged with hyperlinks. 

<img src="FsdocsImages\motley_fool_front_page.png" width="70%" >

*)

(**
Since the transcripts are tagged with a specific hypertext reference (href) (`"/earnings/call-transcripts"`), we can use the `CssSelect` method from FSharp Data to find all elements in a given front page that match the transcript href that we are looking for. After fetching the urls, we can download any transcript we want as an html document using the `HtmlDocument.Load` method, also from FSharp Data.
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
Apart from using the `CssSelect` method to search for transcript urls we can also use it to extract other key information like a company's ticker and exchange as well as the time and date of the earnings call. Since we are not certain that we'll retrieve both a ticker and an exchange from *every* single transcript we parse, we can use match expressions and option types to make sure to return only those matches that contain both a valid ticker and exchange. 
*)

type TranscriptDocument = HtmlDocument

/// Match inner text from html node to a ticker and exchange
let tryTickerExchange (tickerInfo: string option): option<string * string> =
    let split (te:string) =
        match te.Split(":") with
        |[|exchange; ticker|] -> Some (ticker, exchange)
        | _ -> None

    tickerInfo |> Option.map split |> Option.flatten

/// Search for ticker and exchange
let findTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    // Filtering unwanted strings
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> tryTickerExchange

(**
Lets see if we can fetch Tesla's ticker and exchange from its <a href="https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/" target="_blank">latest earnings call</a>:

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
let tryDate (node: HtmlNode option): option<string> =
    node
    |> Option.map cleanDate
    |> Option.flatten

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
    | Some timeNode -> Some (timeNode |> cleanTime)

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

Now that we have working functions for both the date and time of each call, lets combine these functions together and convert the information we have on the date and time of an earnings call to a <a href="https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0" target="_blank">DateTime struct</a> :
*)

/// DateTime converter
let convertToDateTime (dateExpr: string): DateTime =
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.InvariantCulture)

/// Search for and match date and time
let findDateTime (doc: TranscriptDocument): option<DateTime> =
    match findDate doc, findTime doc with
    | Some date, Some time -> 
        $"{date} {time}" |> convertToDateTime |> Some 
    | _ -> None

/// Tesla call DateTime
findDateTime teslaDoc

(*** include-it ***)

(**
## Transcript - Paragraphs

In html, a paragraph is ...
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

So far we have worked with individual functions that take in one single argument, an html transcript document. Since they all work with the `TranscriptDocument` type, we can easily combine these functions together to form one single function that returns all the individual bits of data that we want.
*)

type Transcript = 
    {Ticker : string
     Exchange: string
     Date : DateTime
     Paragraphs : string []}

/// Search for ticker, exchange, date and paragraphs
let parseTrancriptDoc (doc: TranscriptDocument): option<Transcript> =
    let matchExpr =  
        findTickerExchange doc, 
        findDateTime doc, 
        findParagraphs doc    
    
    match matchExpr with
    | Some (ticker, exchange), Some date, paragraphs -> 
        Some { Ticker = ticker
               Exchange = exchange
               Date = date
               Paragraphs = paragraphs}
    | _ -> None

/// Tesla transcript record
let teslaTranscript = 
    parseTrancriptDoc teslaDoc

(*** include-output ***)

(**
## Async methods
*)

let asyncTranscript (url: string) = 
    async {
        let! transcriptDoc = HtmlDocument.AsyncLoad url
        let transcriptRec = parseTrancriptDoc transcriptDoc
        return transcriptRec
        }

let asyncPage (n: int) = 
    async {
        printfn $"{n}"
        let frontPageP = $"https://www.fool.com/earnings-call-transcripts/?page={n}" 
        let! pageDoc = HtmlDocument.AsyncLoad frontPageP

        let transcripts = 
            pageDoc 
            |> findTranscriptUrls 
            |> Seq.map asyncTranscript 
            |> Async.Parallel
            |> Async.RunSynchronously
            |> Seq.choose id
            |> Seq.toArray

        return transcripts }

(**
### Parse Transcript Pages
*)

let exampleTranscripts = 
    [200 .. 225]
    |> Seq.map asyncPage
    |> Async.Sequential// fun xs -> Async.Parallel(xs, 5)
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray

/// Total number of transcripts
printfn $"N: {exampleTranscripts.Length}"

(*** include-output ***)

/// First three transcripts
exampleTranscripts
|> Array.take 3
|> Array.iter (fun transcript -> 
    printfn $" Datetime: {transcript.Date} --- Ticker, Exchange: {transcript.Ticker}, {transcript.Exchange}")

(*** include-output ***)

(**
## Export to json
*)

let TranscriptsToJson (fileName: string) (transcripts: Transcript []) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(*** do-not-eval***)
TranscriptsToJson "data-cache/TranscriptsDemo.json" exampleTranscripts