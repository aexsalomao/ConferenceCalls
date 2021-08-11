(**
# Classifying earnings call transcripts with Naive Bayes
*)

(**
After dowloading earnings transcripts from Motley Fool, we computed the *excess* cumulative return 
of each respective company around its earnings call (see `ReturnsAroundEarnings.fsx`).
We can use these cumulative returns as a way to label or filter "bad" earnings calls from "good" earnings calls. 

In other words, using machine learning lingo, we will use these cumualtive returns to artificially build a target variable, call it `Label`.
Each transcript's `Label` will be modeled as a discriminated union with three cases: `Positive`, `Negative`, and `Neutral`. 
This is a rational decision, as the *excess* cumulative returns are a good proxy for measuring the market's view on earnings calls.
Likewise, the entire feature set will be constructed from the collected transcripts.

Our goal will be to correctly *classify* each transcript's `Label` according to the content provided from each transcript.

*)

(**
## Import packages and load scripts
*)

#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: FSharp.Stats"

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

#load "TranscriptParsing.fsx"
#load "ReturnsAroundEarnings.fsx" 

open TranscriptParsing
open ReturnsAroundEarnings

open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

(**
## Read transcripts from json file
*)

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<AnnouncementDayReturn>>(json)

let rawDataset = 
    let rnd = System.Random()
    readJson ("data-cache/ReturnsAroundEarningsFullSample.json")
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.take 5000
    |> Seq.toArray

(**
## Removing noise from feature set
*)

(**
Some paragraphs are longer than others, and some paragraphs aren't even real paragraphs. 
One-liners like opening and closing remarks add noise to our dataset and should be taken care of.
We can filter for short paragraphs by counting the number of characters present in each paragraph. 
On average, a paragraph contains 3-4 sentences and each sentence contains about 75-100 characters. 
*)

/// Filter paragraphs by character count
let shortParagraphs = 
    rawDataset
    |> Array.collect (fun xs -> 
        xs.Transcript.Paragraphs 
        |> Array.filter (fun paragraph -> paragraph.Length < 100))

shortParagraphs
|> Array.take 10
|> Array.iter (printfn "%s")

(**
## Labeled Transcript
*)

type Label = 
    | Positive
    | Negative
    | Neutral

/// Label from cumulative return
let labelFromRet cumRet thresh = 
    if cumRet > thresh then Positive
    elif cumRet < -thresh then Negative
    elif abs cumRet <= abs thresh then Neutral
    else Neutral

type LabeledTranscript = 
    { TickerExchange: (string * string) 
      EarningsCall: string
      CumulativeReturn: float} with 
      member this.Label = 
        // Label
        labelFromRet this.CumulativeReturn 0.1

let labelDataset (dataset: AnnouncementDayReturn []): LabeledTranscript [] = 
    
    dataset 
    |> Array.map (fun xs -> 
        let tickerExchange = xs.Transcript.Ticker, xs.Transcript.Exchange
        
        let earningsCall = 
            xs.Transcript.Paragraphs
            |> Seq.filter (fun p -> p.Length > 100) // Keep long paragraphs
            |> Seq.skip 1 // Usually another (longer) opening statement from the "Operator"
            |> String.concat (" ")

        { TickerExchange = tickerExchange
          EarningsCall = earningsCall
          CumulativeReturn = xs.CumulativeReturn })

let allTranscripts = labelDataset rawDataset

(**
## Cumulative returns distribution
*)

/// Builds cumulative return histogram
let returnsHist (transcripts: LabeledTranscript []) = 
    let makeChartFromLabel label = 
        let rets = 
            transcripts
            |> Seq.filter (fun xs -> xs.Label = label)
            |> Seq.map (fun xs -> xs.CumulativeReturn)
            |> Seq.toArray
        
        let legendData = 
            let count = rets.Length 
            let avgRet = Array.average rets
            let stDevRet = stDev rets
            $"{label} (N = {count}, Avg. = {Math.Round(avgRet, 4)}, Std. = {Math.Round(stDevRet, 4)})"

        Chart.Histogram (rets |> Array.filter (fun x -> abs x <= 0.5), Name=legendData)

    transcripts
    |> Seq.map (fun xs -> xs.Label)
    |> Seq.distinct
    |> Seq.map makeChartFromLabel
    |> Seq.toArray
    |> Chart.Combine
    |> Chart.withTitle "Excess cumulative returns"
    |> Chart.withSize (1250., 750.)

returnsHist allTranscripts |> Chart.Show

(**
## Naive Bayes Module
*)

type Token = string
type Tokenizer = string -> Token Set
type TokenizedDoc = Token Set

type DocGroup = 
    { Proportion : float
      TokenFrequencies : Map<Token, float> }

let tokenScore (group: DocGroup) (token: Token) =
    if group.TokenFrequencies.ContainsKey token
    then log group.TokenFrequencies.[token]
    else 0.0

let score (document: TokenizedDoc) (group: DocGroup) = 
    let scoreToken = tokenScore group
    log group.Proportion + 
    (document |> Seq.sumBy scoreToken)

let classify (labelGroups: ('Label * DocGroup)[])
             (tokenizer: Tokenizer)
             (txt: string) = 
    let tokenized = tokenizer txt
    labelGroups
    |> Array.maxBy (fun (label, group) -> 
        score tokenized group)
    |> fst

let proportion count total = float count / float total

let laplace count total = float (count+1) / float (total+1)

let countIn (docs: TokenizedDoc seq) (token: Token) =
    docs
    |> Seq.filter (Set.contains token)
    |> Seq.length

let analyze (docsThisLabel: TokenizedDoc seq)
            (nTotalDocs: int)
            (vocabulary: Token Set) =
    let nThisLabel = docsThisLabel |> Seq.length
    let score token =
        let count = countIn docsThisLabel token
        laplace count nThisLabel
    let scoredTokens =
        vocabulary
        |> Set.map (fun token -> token, score token)
        |> Map.ofSeq
    let labelProportion = proportion nThisLabel nTotalDocs

    { Proportion = labelProportion 
      TokenFrequencies = scoredTokens }

let learn (docs: ('Label * string)[])
          (tokenizer: Tokenizer)
          (vocabulary: Token Set) =
    let total = docs.Length
    docs
    |> PSeq.map (fun (label, docString) -> label, tokenizer docString)
    |> PSeq.groupBy fst
    |> PSeq.map (fun (label, (xs: seq<'Label * TokenizedDoc>)) -> 
        let tokenizedDocs = xs |> Seq.map snd
        label, analyze tokenizedDocs total vocabulary)
    |> PSeq.toArray

let train (docs: ('Label * string)[]) 
          (tokenizer: Tokenizer)
          (vocabulary: Token Set) =
    let labelGroups = learn docs tokenizer vocabulary
    let classifier = classify labelGroups tokenizer
    classifier

(**
## Dataset split
*)

(**
80% -> training
20% -> validation
*)

let training, validation =
    let posOrNeg = 
        allTranscripts
        |> Seq.filter (fun xs -> xs.Label <> Neutral)
        |> Seq.map (fun xs -> (xs.Label, xs.EarningsCall))
        |> Seq.toArray

    let cutoff = (float posOrNeg.Length) * 0.8 |> int

    posOrNeg.[.. cutoff], posOrNeg.[cutoff + 1 ..]


training.Length
validation.Length

(**
## Tokenizer
*)

let vocabulary (tokenizer: Tokenizer) (corpus: string seq) = 
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

let onlyWords = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")

let tokenizeAllWords (text: string) = 
    text
    |> onlyWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let applyTokenizer (tokenizer: Tokenizer) =
    training
    |> Seq.map snd
    |> vocabulary tokenizer

(**
## Evaluate performance by tokenizer
*)

let evaluate (tokenizer: Tokenizer) (tokens: Token Set) = 
    let classifier = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (label, text) -> 
        if label = classifier text then 1.0 
        else 0.)
    |> printfn "Correctly classified: %.4f"

(**
## N-Grams (DocTransformer)
*)

let nGrams (n: int) (text: string): string [] = 
    let candidateNGrams = 
        text.Split(" ")
        |> Array.windowed n

    let getOnlyWords words =
        let onlyWords word: option<Match> = 
            let candidateMatch = onlyWords.Match word
            if candidateMatch.Success then Some candidateMatch 
            else None

        words 
        |> Seq.choose onlyWords
        |> Seq.map (fun m -> m.Value) 

    let tryNGram words = 
        if (Seq.length words = n) then Some (String.concat(" ") words)
        else None

    candidateNGrams
    |> Seq.map getOnlyWords
    |> Seq.choose tryNGram
    |> Seq.toArray

let twoGramsTokenizer (text: string): Set<string> = 
    nGrams 2 text
    |> Set.ofArray

let threeGramsTokenizer (text: string): Set<string> = 
    nGrams 3 text
    |> Set

/// Some tests
let woodpecker = "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."
let twoGramsTest = twoGramsTokenizer woodpecker
let threeGramsTest = threeGramsTokenizer woodpecker

(**
## Term Frequency (TF)
$tf_{t,d} = \frac{n_{t,d}}{number of terms in a document}$
$n_{t,d}$ : number of times a term *t* is present in a document *d*.
*)

type DocTransfromer = string -> string []

type Stat = 
    | Tf of float
    | Idf of float
    | TfIdf of float

type TermStat = 
    { Term: string 
      Stat: Stat } 

let tf (docTransformer: DocTransfromer) (doc: string): TermStat [] =
    let terms = docTransformer doc
    let nTerms = float terms.Length

    terms
    |> Seq.countBy id
    |> Seq.map (fun (term, count) ->
        let tf = (float count) / (nTerms)
        {Term = term; Stat = Tf tf})
    |> Seq.toArray

(**
## Inverse document frequency (IDF)
$IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})
*)

let idf (docTransfromer: DocTransfromer) (docs : string []): Map<string, TermStat> = 
    let n = docs.Length
    
    let numberOfDocsByTerm = 
        docs
        |> Seq.map docTransfromer
        |> Seq.collect Seq.distinct
        |> Seq.countBy id
      
    numberOfDocsByTerm
    |> Seq.map (fun (term, docsWithTerm) -> 
        let idf = log (float n / float docsWithTerm)
        term, {Term = term; Stat = Idf idf})
    |> Map

(**
## Term frequency - Inverse Document Frequency
$TF-IDF(t, d) = TF(t, d) * IDF(t)$
A high weight in tf–idf is reached by a high term frequency (in the given document) and a low document frequency of the term in the whole collection of documents; 
the weights hence tend to filter out common terms.
*)

let tfIdf (docTransformer : DocTransfromer)
          (inverseDocFreq: Map<string, TermStat>)
          (doc: string): TermStat [] =

   let getTfIdf termTf = 
       let computeTfIdf termIdf = 
            match termTf.Stat, termIdf.Stat with
            | Tf tf, Idf idf -> 
                let tfIdf = tf * idf
                Some { Term = termIdf.Term 
                       Stat = TfIdf idf }
            | _ -> None

       inverseDocFreq.TryFind termTf.Term
       |> Option.map computeTfIdf
       |> Option.flatten
            
   doc
   |> tf docTransformer
   |> Array.choose getTfIdf

(**
## Tf-Idf tests
*)

// Test docs
let doc1 = snd training.[0]
let allDocs = training |> Array.map snd |> Seq.toArray

/// Tests
let twoGrams = nGrams 2 // DocTransformer
let tfDoc1 = tf twoGrams doc1 // Tf test
let idfTwoGrams = idf twoGrams allDocs // Idf test
let tfIdfDoc1 = tfIdf twoGrams idfTwoGrams doc1 // TfIdf test

(**
# Word bar chart - Doc1
*)

let lowTfIdfWords = 
    tfIdfDoc1
    |> Seq.sortBy (fun xs -> xs.Stat)
    |> Seq.distinctBy (fun xs -> xs.Stat)
    |> Seq.take 25
    |> Seq.toArray

let highTfIdfWords = 
    tfIdfDoc1
    |> Seq.sortByDescending (fun xs -> xs.Stat)
    |> Seq.distinctBy (fun xs -> xs.Stat)
    |> Seq.take 25
    |> Seq.rev
    |> Seq.toArray

let wordBarChart (words : TermStat []) (title: string) = 
    words
    |> Seq.map (fun xs -> 
        match xs.Term, xs.Stat with 
        | term, TfIdf tfIdf when tfIdf > 0.-> 
            Some(term, tfIdf)
        | _ -> None)
    |> Seq.choose id
    |> Seq.toArray
    |> Chart.Column
    |> Chart.withTitle title
    |> Chart.withSize (1250., 500.)

/// wordBarChart lowTfIdfWords "Low Tf-Idf words (Common words)" |> Chart.Show
/// wordBarChart highTfIdfWords "High Tf-Idf words (Relevant words)" |> Chart.Show

(**
# Tf-idf histogram
*)

/// TwoGrams
let allDocsTwoGramsTfIdf = 
    allDocs
    |> Array.collect (fun doc -> tfIdf twoGrams idfTwoGrams doc)

let description =
    let heading = "Comments"
    let description = 
        "Very rare words do convey meaning, but their added computational cost in expanding 
         the set of features that must be considered often exceeds their diagnostic value.
         An approach that excludes both common and rare words and has proven very useful in practice 
         is filtering by 'term frequency - inverse document frequency' (tf-idf).
         A high weight in tf-idf is reached by a high term frequency (in the given document) and 
         a low document frequency of the term in the whole collection of documents; 
         the weights hence tend to filter out common terms."

    ChartDescription.create heading description

let tfIdfHistogram (terms: TermStat [])
                   (description: ChartDescription)
                   (title: string) =
    terms
    |> Seq.map (fun xs -> 
        match xs.Stat with
        | TfIdf tfIdf -> Some tfIdf
        | _ -> None)
    |> Seq.choose id
    // For vizualization purposes
    |> Seq.filter (fun x -> x < 0.005)
    |> Seq.toArray
    |> Chart.Histogram
    |> Chart.withTitle title
    |> Chart.withX_AxisStyle "Term frequency - inverse document frequency"
    |> Chart.withY_AxisStyle "Frequency"
    |> Chart.WithDescription (description)
    |> Chart.withSize (600., 600.)

let twoGramsHist = tfIdfHistogram allDocsTwoGramsTfIdf description "2-Grams"

/// Chart.Show twoGramsHist

/// twoGramsHist |> Chart.SaveHtmlAs "data-cache/2GramsHist"

(**
# Tf-Idf + NGrams Tokenizer
*)

let tfIdfNGramsTokenizer (docTransformer: DocTransfromer) 
                         (inverseDocFreq: Map<string, TermStat>)
                         (tfIdfThresh : float) 
                         (doc: string)= 
    let tdIdfofDoc = 
        tfIdf docTransformer inverseDocFreq doc
        |> Seq.map (fun xs -> xs.Term, xs)
        |> Map
    
    let relevantTerm term = 
        tdIdfofDoc.TryFind term
        |> Option.map (fun term -> 
            match term.Stat with
            | TfIdf tfIdf when tfIdf > tfIdfThresh -> Some term.Term
            | _ -> None )
        |> Option.flatten
        
    doc
    |> docTransformer
    |> Seq.distinct
    |> Seq.choose relevantTerm
    |> Set

(**
# Tokenizer evaluation
## Simple Tokenizer (Regex single word)
*)

let allTokens = applyTokenizer tokenizeAllWords
evaluate tokenizeAllWords allTokens

(**
### Tf-Idf + NGrams Tokenizer
*)

/// Evaluate TfIdf + twoGrams Tokenizer 
let twoGramsTransformer = nGrams 2
let trainingTwoGramsIdf = idf twoGramsTransformer allDocs
let tfIdfTwoGramsTokenzier = tfIdfNGramsTokenizer twoGramsTransformer trainingTwoGramsIdf 0.07

let tfIdfTwoGramsTokens = applyTokenizer tfIdfTwoGramsTokenzier
evaluate tfIdfTwoGramsTokenzier tfIdfTwoGramsTokens