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
## Generic Url - Transcript
- In order to download or parse individual transcripts, we first need to find a way to fetch individual transcript urls. 
Fortunately, we can fetch multiple transcript urls from [motley fool's front page]("https://www.fool.com/earnings-call-transcripts/?page=1") 
using the `CssSelect` method from FSharp Data. 
Since the transcripts are tagged with a specific hypertext referece (href) (`"/earnings/call-transcripts"`), 
we can use the `CssSelect` method to find all elements in a given front page that match the transcript href that we are looking for.

- After fetching the individual urls, we can download any transcript we want as an html document using the `HtmlDocument.Load` method, also from FSharp Data.
*)

type FrontPageDocument = HtmlDocument

let TryHref (node: HtmlNode): string option =
    match node.TryGetAttribute("href") with
    | None -> None
    | Some attrib -> Some ("https://www.fool.com" + attrib.Value())

let FindTranscriptUrls (pageDoc: FrontPageDocument): seq<string> = 
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose TryHref
    
(** 
Lets take a look at the first three call transcript urls `CssSelect` was able to match:
*)

let exampleFrontPage = "https://www.fool.com/earnings-call-transcripts/?page=1"

// (*** include-fsi-output ***)
exampleFrontPage
|> HtmlDocument.Load 
|> FindTranscriptUrls
|> Seq.take 3
|> Seq.iter (printfn "%s")

(**
## Transcript - Ticker & Exchange
- Apart from using the `CssSelect` method to search for transcript urls we can also use it to extract other key information like a company's ticker and/or exchange. 

- Since we are not certain that we'll retrieve both a ticker and an exchange from *every* single transcript we parse, 
we can use match expressions to make sure to return only those matches that contain both a valid ticker and exchange. 
*)

type TranscriptDocument = HtmlDocument

let TryTickerExchange (tickerInfo: string option): option<string * string> =
    match tickerInfo with
    | Some te -> 
                 match te.Split(":") with
                 |[|exchange; ticker|] -> Some (ticker, exchange)
                 | _ -> None
    | _ -> None

let FindTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> TryTickerExchange

(** 
Lets see if we can fetch Tesla's ticker and exchange from its [lastest earnings call](https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/): 
![GitHub Logo](/ScreenCaptures/tesla_motley_fool.jpeg)

*) 

let teslaTranscriptUrl = "https://www.fool.com/earnings/call-transcripts/2021/07/27/tesla-tsla-q2-2021-earnings-call-transcript/"

teslaTranscriptUrl
|> HtmlDocument.Load
|> FindTickerExchange

(**
## Transcript - Date
*)

// Date
let CleanDate (node: HtmlNode): option<string> = 
    let dateSplit = node.InnerText().ToUpperInvariant().Replace(".", "").Replace(",", "").Trim().Split(" ")
    match dateSplit with
    |[|month; day; year|] -> Some ($"{month.[..2]} {day} {year}") 
    | _ -> None

let TryDate (node: HtmlNode option): option<string> =
    match node with
    | None -> None 
    | Some dateNode -> 
        let cleanDate = (dateNode |> CleanDate)
        if cleanDate.IsSome then Some (cleanDate.Value) else None

let FindDate (doc: TranscriptDocument): option<string>=
    doc.CssSelect("span[id='date']")
    |> Seq.tryExactlyOne
    |> TryDate

teslaTranscriptUrl
|> HtmlDocument.Load
|> FindDate

(**
## Transcript - Time
*)

// Time
let CleanTime (node: HtmlNode ) = 
    node.InnerText().ToUpperInvariant().Replace(".", "").Replace("ET", "").Trim()

let TryTime (node: HtmlNode option): option<string> =
    match node with
    | None -> None
    | Some timeNode -> Some (timeNode |> CleanTime)

let FindTime (doc: TranscriptDocument) =
    doc.CssSelect("em[id='time']")
    |> Seq.tryExactlyOne
    |> TryTime

teslaTranscriptUrl
|> HtmlDocument.Load
|> FindTime

(**
## Transcript - Datetime
*)

// DateTime
let ConvertToDatetime (dateExpr: string): DateTime =
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.CurrentCulture)

let FindDateTime (doc: HtmlDocument): option<DateTime> =
    match (doc |> FindDate), (doc |> FindTime) with
    | Some date, Some time -> Some ($"{date} {time}" |> ConvertToDatetime) 
    | _ -> None

teslaTranscriptUrl
|> HtmlDocument.Load
|> FindDateTime

(**
# Transcript - Paragraphs
*)

let FindParagraphs (transcriptDoc: HtmlDocument) = 
    transcriptDoc.CssSelect("p")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> x <> "")
    |> String.concat(" ")

teslaTranscriptUrl
|> HtmlDocument.Load
|> FindParagraphs

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

let ParseTrancriptDocument (doc: TranscriptDocument): option<Transcript> =
    let matchExpr =  (doc |> FindTickerExchange), (doc |> FindDateTime), (doc |> FindParagraphs)      
    
    match matchExpr with
    | Some (ticker, exchange), Some date, paragraphs -> Some { Ticker = ticker
                                                               Exchange = exchange
                                                               Date = date
                                                               Paragraphs = paragraphs}
    | _ -> None

teslaTranscriptUrl
|> HtmlDocument.Load
|> ParseTrancriptDocument

(**
## Async methods
*)

let asyncTranscript (url: string) = 
    async {
        let! transcriptDocument = url |> HtmlDocument.AsyncLoad
        let transcriptInfo = transcriptDocument |> ParseTrancriptDocument
        return transcriptInfo
        }

let asyncPage (n: int) = 
    async {
        let frontPageP = $"https://www.fool.com/earnings-call-transcripts/?page={n}" 
        let! pageDoc = frontPageP |> HtmlDocument.AsyncLoad 

        let transcripts = 
            pageDoc 
            |> FindTranscriptUrls 
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