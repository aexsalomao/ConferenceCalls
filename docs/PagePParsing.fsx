(**
# Parsing Motley Fool
- The objective of this .fsx script to give a few examples on how to parse html documents with F#. More specifically, 
we will be concerned with parsing earnings calls transcripts from [Motley Fool](https://www.fool.com/earnings-call-transcripts/).

- Before getting started, lets download the [FSharp Data](https://fsprojects.github.io/FSharp.Data/) package
using .NET's package manager [NuGet](https://www.nuget.org/):
*)

#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json, 13.0.1"

open FSharp.Data
open Newtonsoft.Json
open System

(**
## Transcript - Url
- In order to download or parse individual transcripts, we first need to find a way to fetch individual transcript urls. 
Fortunately, we can fetch multiple transcript urls from [motley fool's front page]("https://www.fool.com/earnings-call-transcripts/?page=1") using the `CssSelect` method from FSharp Data. 
*)

(**
<img src="Images\motley_fool_front_page.png" width="533" height="366">
*)

(**
- Since the transcripts are tagged with a specific hypertext referece (href) (`"/earnings/call-transcripts"`), we can use the `CssSelect` method to find all elements in a given front page that match the transcript href that we are looking for. After fetching the individual urls, we can download any transcript we want as an html document using the `HtmlDocument.Load` method, also from FSharp Data.
*)

type FrontPageDocument = HtmlDocument

let tryHref (node: HtmlNode): string option =
    match node.TryGetAttribute("href") with
    | None -> None
    | Some attrib -> Some ("https://www.fool.com" + attrib.Value())

let findTranscriptUrls (pageDoc: FrontPageDocument): seq<string> = 
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose tryHref
    
(** 
Lets take a look at the first three call transcript urls `CssSelect` was able to match:
*)

let exampleFrontPage = "https://www.fool.com/earnings-call-transcripts/?page=1"

exampleFrontPage
|> HtmlDocument.Load 
|> findTranscriptUrls
|> Seq.take 3
|> Seq.iter (printfn "%s")

(*** include-output ***)


(**
## Transcript - Ticker & Exchange
- Apart from using the `CssSelect` method to search for transcript urls we can also use it to extract other key information like a company's ticker and/or exchange. 
- Since we are not certain that we'll retrieve both a ticker and an exchange from *every* single transcript we parse, we can use match expressions to make sure to return only those matches that contain both a valid ticker and exchange. 
*)

type TranscriptDocument = HtmlDocument

let tryTickerExchange (tickerInfo: string option): option<string * string> =
    match tickerInfo with
    | Some te -> 
                 match te.Split(":") with
                 |[|exchange; ticker|] -> Some (ticker, exchange)
                 | _ -> None
    | _ -> None

let findTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> tryTickerExchange

(**
Lets see if we can fetch Tesla's ticker and exchange from its <a href="https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/" target="_blank">latest earnings call</a>
*)

(**
<img src="Images\tesla_motley_fool.png" width="533" height="366">
*) 

let teslaTranscriptUrl = "https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/"

teslaTranscriptUrl
|> HtmlDocument.Load
|> findTickerExchange

(*** include-it ***)

(**
## Transcript - Date
*)

// Date
let cleanDate (node: HtmlNode): option<string> = 
    let dateSplit = node.InnerText().ToUpperInvariant().Replace(".", "").Replace(",", "").Trim().Split(" ")
    match dateSplit with
    |[|month; day; year|] -> Some ($"{month.[..2]} {day} {year}") 
    | _ -> None

let tryDate (node: HtmlNode option): option<string> =
    match node with
    | None -> None 
    | Some dateNode -> 
        let cleanDate = (dateNode |> cleanDate)
        if cleanDate.IsSome then Some (cleanDate.Value) else None

let findDate (doc: TranscriptDocument): option<string>=
    doc.CssSelect("span[id='date']")
    |> Seq.tryExactlyOne
    |> tryDate

teslaTranscriptUrl
|> HtmlDocument.Load
|> findDate

(**
## Transcript - Time
*)

// Time
let cleanTime (node: HtmlNode ) = 
    node.InnerText().ToUpperInvariant().Replace(".", "").Replace("ET", "").Trim()

let tryTime (node: HtmlNode option): option<string> =
    match node with
    | None -> None
    | Some timeNode -> Some (timeNode |> cleanTime)

let findTime (doc: TranscriptDocument) =
    doc.CssSelect("em[id='time']")
    |> Seq.tryExactlyOne
    |> tryTime

teslaTranscriptUrl
|> HtmlDocument.Load
|> findTime

(**
## Transcript - Datetime
*)

// DateTime
let convertToDatetime (dateExpr: string): DateTime =
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.CurrentCulture)

let findDateTime (doc: HtmlDocument): option<DateTime> =
    match (doc |> findDate), (doc |> findTime) with
    | Some date, Some time -> Some ($"{date} {time}" |> convertToDatetime) 
    | _ -> None

teslaTranscriptUrl
|> HtmlDocument.Load
|> findDateTime

(**
# Transcript - Paragraphs
*)

let findParagraphs (transcriptDoc: HtmlDocument) = 
    transcriptDoc.CssSelect("p")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> x <> "")
    |> String.concat(" ")

teslaTranscriptUrl
|> HtmlDocument.Load
|> findParagraphs

(**
# Transcript Record
- So far we have worked with individual functions that take in one single argument, an html transcript document. 
Since they all work with the `TranscriptDocument` type, we can easily combine these functions together to form one 
single function that returns all the individuals groups of data that we want.
*)

type Transcript = 
    {Ticker : string
     Exchange: string
     Date : DateTime
     Paragraphs : string}

let parseTrancriptDocument (doc: TranscriptDocument): option<Transcript> =
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

teslaTranscriptUrl
|> HtmlDocument.Load
|> parseTrancriptDocument
|> fun transcript ->
    match transcript with
    | None -> ()
    | Some transcript -> 
        let actual =
            transcript.Paragraphs.Split(".")
            |> Array.filter(fun x -> x.Trim().StartsWith("Good day"))
            |> String.concat(".")
        printfn $"""Datetime: {transcript.Date}
Ticker and Exchange: {transcript.Ticker}, {transcript.Exchange} 
Paragraphs: {actual.[0 .. 300]}"""
(*** include-output ***)

(**
## Async methods
*)

let asyncTranscript (url: string) = 
    async {
        let! transcriptDocument = url |> HtmlDocument.AsyncLoad
        let transcriptInfo = transcriptDocument |> parseTrancriptDocument
        return transcriptInfo
        }

let asyncPage (n: int) = 
    async {
        let frontPageP = $"https://www.fool.com/earnings-call-transcripts/?page={n}" 
        let! pageDoc = frontPageP |> HtmlDocument.AsyncLoad 

        let transcripts = 
            pageDoc 
            |> findTranscriptUrls 
            |> Seq.map asyncTranscript 
            |> fun xs -> Async.Parallel(xs, 5)
            |> Async.RunSynchronously
            |> Seq.choose (
             function
             | None -> None
             | Some t -> Some t)

        return transcripts
        }

(**
# Parse Transcript Pages
*)

let asyncTest1to10 = 
    [1 .. 10]
    |> Seq.map asyncPage
    |> fun xs -> Async.Parallel(xs, 5)
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray

asyncTest1to10
|> Seq.take 3
|> Seq.iter (fun transcript -> 
    printfn $" Datetime: {transcript.Date} --- Ticker and Exchange: {transcript.Ticker}, {transcript.Exchange} --- Paragraps: {transcript.Paragraphs.[377 .. 430]}")
(**
# Export to json
*)

let TranscriptsToJson (transcripts: Transcript [], fileName: string) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(*
TranscriptsToJson (asynchTest1to100, "data-cache/Motley100.json")
TranscriptsToJson (asyncTest1to300, "data-cache/EarningsCallTest200.json")
TranscriptsToJson (asyncTest, "data-cache/EarningsCallTest.json")
TranscriptsToJson (asyncPagesContent, "EarningsTranscripts.json")
*)