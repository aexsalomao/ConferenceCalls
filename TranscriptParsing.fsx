#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"

open FSharp.Data
open Newtonsoft.Json
open System

(**
# Generic Page - Urls
*)

type FrontPageDocument = HtmlDocument

/// Match html node with "href" attribute
let tryHref (node: HtmlNode): string option =
    match node.TryGetAttribute("href") with
    | None -> None
    | Some attrib -> Some ("https://www.fool.com" + attrib.Value())

/// Search for transcript urls
let findTranscriptUrls (pageDoc: FrontPageDocument): string [] =  
    pageDoc.CssSelect("a[href^='/earnings/call-transcripts']")
    |> Seq.choose tryHref
    |> Seq.toArray
    
(**
# Generic Transcript - Ticker
*)

type TranscriptDocument = HtmlDocument

/// Match inner text from html node to a ticker and exchange
let tryTickerExchange (tickerInfo: string option): option<string * string> =
    match tickerInfo with
    | Some te -> 
                 match te.Split(":") with
                 |[|exchange; ticker|] -> Some (ticker, exchange)
                 | _ -> None
    | _ -> None

/// Search for ticker and exchange
let findTickerExchange (doc: TranscriptDocument): option<string * string> = 
    doc.CssSelect("span[class='ticker']")
    |> Seq.map (fun x -> x.InnerText().Trim())
    // Filtering unwanted strings
    |> Seq.filter (fun x -> not (x.Contains("(")))
    |> Seq.tryExactlyOne
    |> tryTickerExchange

(**
## Transcript - Date & Time

### Date
*)

/// Format date string
let cleanDate (node: HtmlNode): option<string> = 
    let dateSplit = node.InnerText().ToUpperInvariant().Replace(".", "").Replace(",", "").Trim().Split(" ")
    match dateSplit with
    |[|month; day; year|] -> Some ($"{month.[..2]} {day} {year}") 
    | _ -> None

/// Match html node with some date
let tryDate (node: HtmlNode option): option<string> =
    match node with
    | None -> None 
    | Some dateNode -> 
        let cleanDate = (dateNode |> cleanDate)
        if cleanDate.IsSome then Some (cleanDate.Value) else None

/// Search for transcript date
let findDate (doc: TranscriptDocument): option<string>=
    doc.CssSelect("span[id='date']")
    |> Seq.tryExactlyOne
    |> tryDate

(**
### Time
*)

/// Format time string
let cleanTime (node: HtmlNode ) = 
    node.InnerText().ToUpperInvariant().Replace(".", "").Replace("ET", "").Trim()

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

(**
### DateTime
*)

// DateTime converter
let convertToDateTime (dateExpr: string): DateTime =
    let dateFormat = "MMM d yyyy h:mm tt"
    DateTime.ParseExact(dateExpr, dateFormat, System.Globalization.CultureInfo.CurrentCulture)

// Search for and match date and time
let findDateTime (doc: TranscriptDocument): option<DateTime> =
    match (doc |> findDate), (doc |> findTime) with
    | Some date, Some time -> Some ($"{date} {time}" |> convertToDateTime) 
    | _ -> None

(**
# Generic Transcript - Paragraphs
*)

let findParagraphs (doc: HtmlDocument): string [] = 
    doc.CssSelect("p")
    |> Seq.map (fun x -> x.InnerText().Trim())
    |> Seq.filter (fun x -> x <> "")
    // Skip first 5 paragraphs
    |> Seq.skip 5
    |> Seq.toArray

(**
# Transcript Record
- Adding more structure ...
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
            |> fun xs -> Async.Parallel(xs, 5)
            |> Async.RunSynchronously
            |> Seq.choose (
             function
             | None -> None
             | Some t -> Some t)
            |> Seq.toArray

        return transcripts
        }

(**
# Parse Transcript Pages
- Fix this ?

let async1to75 = 
    [1 .. 75]
    |> Seq.map asyncPage
    |> fun xs -> Async.Parallel(xs, 5)
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray

let async76to150 = 
    [76 .. 150]
    |> Seq.map asyncPage
    |> fun xs -> Async.Parallel(xs, 5)
    |> Async.RunSynchronously
    |> Array.collect Seq.toArray

let async1to150 = Array.append async1to75 async76to150
*)

(**
# Export to json
*)

let TranscriptsToJson (transcripts: Transcript [], fileName: string) = 
    JsonConvert.SerializeObject(transcripts)
    |> fun json -> IO.File.WriteAllText(fileName, json)

(*
TranscriptsToJson (async1to150, "data-cache/Transcripts3000.json")
*)