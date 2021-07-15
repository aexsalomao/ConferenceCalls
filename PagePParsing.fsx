#r "nuget: FSharp.Data, 4.1.1"
#r "nuget: Newtonsoft.Json, 13.0.1"

open FSharp.Data
open Newtonsoft.Json
open System
open System.Globalization
open System.Xml

(**
# Generic Page - Urls
*)

let tryHref (node: HtmlNode) =
    let urlHome = "https://www.fool.com"
    node.TryGetAttribute("href")
    |> Option.map (fun x -> urlHome + x.Value())

let TranscriptUrls (pageDoc: HtmlDocument): string list= 
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose tryHref
    |> Seq.toList

(**
# Generic Transcript - Ticker
*)

type CssTickerInfo = 
    {CssTicker : string
     CssExchange: string}

let tickerExchangeTuple (tickerInfo: string option)=
    tickerInfo
    |> function
    | Some te -> te.Split(":") 
                 |> function
                 | [|exchange; ticker|] -> Some ({CssTicker = ticker
                                                  CssExchange = exchange})
                 | _ -> None
    | _ -> None

let TickerAndExchange (transcriptDoc: HtmlDocument) = 
    transcriptDoc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> tickerExchangeTuple

(**
# Generic Transcript - Date, Time
*)

// Date
let cleanDate (node: HtmlNode) =
    let dateSplit = node.InnerText().ToUpperInvariant().Replace(".", "").Replace(",", "").Trim().Split(" ")
    match dateSplit with
    |[|month; day; year|] -> Some ($"{month.[..2]} {day} {year}") 
    | _ -> None

let TranscriptDate (transcriptDoc: HtmlDocument) =
    transcriptDoc.CssSelect("span[id='date']")
    |> Seq.choose cleanDate
    |> Seq.tryExactlyOne

// Time
let cleanTime (node: HtmlNode) =
    node.InnerText().ToUpperInvariant().Replace(".", "").Replace("ET", "").Trim()

let TranscriptTime (transcriptDoc: HtmlDocument) =
    transcriptDoc.CssSelect("em[id='time']")
    |> Seq.map cleanTime
    |> Seq.tryExactlyOne

// DateTime
let exprToDateTime (dateExpr: string) =
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.CurrentCulture)

let TranscriptDateTime (doc: HtmlDocument) =
    let timeT = doc |> TranscriptTime 
    let dateT = doc |> TranscriptDate
    
    (dateT, timeT)
    |> function
    | Some d, Some t -> Some ($"{d} {t}" |> exprToDateTime) 
    | _ -> None

(**
# Generic Transcript - Paragraphs
*)

let Paragraphs (transcriptDoc: HtmlDocument) = 
    transcriptDoc.CssSelect("p")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> x <> "")
    |> Seq.toList

(**
# Transcript Record
- Adding more structure ...
*)

type Transcript = 
    {Ticker : string
     Exchange: string
     Date : DateTime
     Paragraphs : string list}

let MatchTranscript =
    function
    | Some {CssTicker = ticker; CssExchange = exchange}, Some date, paragraphs -> Some { Ticker = ticker
                                                                                         Exchange = exchange
                                                                                         Date = date
                                                                                         Paragraphs = paragraphs}
    | _ -> None

let parseTrancript (transcript: HtmlDocument) = 
    
    let tickerAndExchange = transcript |> TickerAndExchange
    let period = transcript |> TranscriptDateTime
    let paragraphs = transcript |> Paragraphs
    
    (tickerAndExchange, period, paragraphs) 
    |> MatchTranscript

let asyncTranscript (url: string) = 
    async {
        let! doc = url |> HtmlDocument.AsyncLoad
        let content = doc |> parseTrancript
        return content
        }

let asyncPage (n: int) = 
    async {
        let papeP = $"https://www.fool.com/earnings-call-transcripts/?page={n}"
        
        let! pageDoc = papeP|> HtmlDocument.AsyncLoad 

        let transcripts = 
            pageDoc 
            |> TranscriptUrls 
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


(**
let asyncTest1to300 = 
    [1 .. 300]
    |> Seq.map asyncPage
    |> fun xs -> Async.Parallel(xs, 5)
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray



let asyncTest = 
    [1 .. 20]
    |> Seq.map asyncPage
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray

let asyncPagesContent =
    [1 .. 144]
    |> Seq.map asyncPage
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray
*)

(**
# Export to json
*)

let TranscriptsToJson (transcripts: Transcript [], fileName: string) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(**

TranscriptsToJson (asyncTest1to300, "data-cache/EarningsCallTest200.json")


TranscriptsToJson (asyncTest, "data-cache/EarningsCallTest.json")

TranscriptsToJson (asyncPagesContent, "EarningsTranscripts.json")
*)

(**
# Objective: identify good/bad earnings news.
- First 10 pages (200 calls)

- To-do:
    - Function to get the Date and time of the earnings call
    - See 5th paragraph
*)






