(**
## Parsing Motley Fool

- The objective of this .fsx script is to give a few examples on how to parse html documents with F#.
- More specifically, we will be concerned with parsing earnings announcements transcripts from Motley Fool (insert link here).
*)

#r "nuget: FSharp.Data "
#r "nuget: Newtonsoft.Json, 13.0.1"

open FSharp.Data
open Newtonsoft.Json
open System

(**
## Generic Url - Transcript

- Before parsing individual transcripts, we first need to find a way to find the actual transcript urls. 
Fortunately, we can fetch individual transcript urls from motley fool's front page .

- Example front page: "https://www.fool.com/earnings-call-transcripts/?page=1"
*)

let tryWithHref (node: HtmlNode): string option =
    match node.TryGetAttribute("href") with
    | None -> None
    | Some attrib -> Some ("https://www.fool.com" + attrib.Value())

let FindTranscriptUrls (pageDoc: HtmlDocument): string list = 
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose tryWithHref
    |> Seq.toList

(** Lets take a look at the first 5 urls *)
let exampleFrontPage = "https://www.fool.com/earnings-call-transcripts/?page=1"

let fiveTranscriptUrls: string list = 
    exampleFrontPage
    |> HtmlDocument.Load 
    |> FindTranscriptUrls
    |> List.take 5

// (*** include-fsi-output ***)
fiveTranscriptUrls 
|> List.iter (printfn "%s")

(**
## Generic Transcript - Ticker & Exchange

- Now that we have our transcript urls we can simply use the same `HtmlDocument.Load` method on all urls.
- We can now use the `CssSelect` method to search for key information like a company's ticker and exchange. 
Since we'll use this information later on, it is best if we place this information into a `TickerExchange` reccord.
*)

type TickerExchange = 
    { Ticker : string
      Exchange : string}

(** Since we are not certain that we'll retrieve a ticker and exchange from *every* single transcript (html document), 
we can choose to only save those reccords that contain both a ticker and exchange.
This sounds like a task for match expressions and option types. *)

let tryFindTickerExchange (tickerInfo: string option): TickerExchange option =
    match tickerInfo with
    | Some te -> match te.Split(":") with
                 |[|exchange; ticker|] -> Some ({ Ticker = ticker
                                                  Exchange = exchange})
                 | _ -> None
    | _ -> None

let FindTickerExchange (transcriptDoc: HtmlDocument) = 
    transcriptDoc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> tryFindTickerExchange

(** Lets see if we manage to fetch the ticker and exchange of our first transcript **) 

(** We can then place that information inside a reccord*)

(**
# Generic Transcript - Date & Time
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
    |> Seq.fold (fun r s -> r + s + " ") ""
    |> string

(**
# Transcript Record
- Adding more structure ...
*)

type Transcript = 
    {Ticker : string
     Exchange: string
     Date : DateTime
     Paragraphs : string}

let MatchTranscript =
    function
    | Some {Ticker = ticker; CssExchange = exchange}, Some date, paragraphs -> Some { Ticker = ticker
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
        printfn $"Page: {n}"
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

(*
let asyncTest1to100 = 
    [1 .. 100]
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

(*
TranscriptsToJson (asynchTest1to100, "data-cache/Motley100.json")
TranscriptsToJson (asyncTest1to300, "data-cache/EarningsCallTest200.json")
TranscriptsToJson (asyncTest, "data-cache/EarningsCallTest.json")
TranscriptsToJson (asyncPagesContent, "EarningsTranscripts.json")
*)