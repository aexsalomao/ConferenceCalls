(**
---
title: Classifying earnings call transcripts with Naive Bayes
category: Scripts
categoryindex: 3
index: 2
---
*)

(**
# Classifying earnings call transcripts with Naive Bayes
*)

(**
After dowloading earnings transcripts from Motley Fool, we proceeded to computed 
the *excess* cumulative returns of each respective company around its earnings call 
in `ReturnsAroundEarnings.fsx`. We can now use the excess cumulative returns we have 
computed as a *proxy* that is meant to measure the market's overall sentiment towards 
a given earnings call.

More specifically, we'll place company transcripts into buckets according to some 
pre-specified excess cumulative return threshold:

- The market sentiment of each transcript can be modeled as a discriminated union, 
call it `MktSentiment`, with three cases: `Positive`, `Negative`, and `Neutral`. 

- In machine learning lingo, by labelling each transcript according to some threshold,
we'll be transforming our variable of interest from numeric to categorical. 
In other words, we'll be transforming a regression problem into a classification problem.

There are many machine learning algorithms to choose from when trying to solve a binary or multi-classification problem.
Due to its simplicity and intuitive framework, a Naive Bayes[] classifier is often a good place to start.
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

let fullRawSample = 
    readJson ("data-cache/ReturnsAroundEarningsFullSample.json")
    |> Seq.toArray

let randRawSample =
    let rnd = System.Random()
    fullRawSample
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
Since we want to eliminate short paragraphs, we will filter them out by their character count (<100).
*)

/// Filter paragraphs by character count
let shortParagraphs = 
    randRawSample
    |> Array.collect (fun xs -> 
        xs.Transcript.Paragraphs 
        |> Array.filter (fun paragraph -> paragraph.Length < 100))

shortParagraphs
|> Array.take 5
|> Array.iter (printfn "%s")
(*** include-output***)

(**
## Labelling transcripts: Market sentiment
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

let allTranscripts = labelDataset randRawSample

(**
## Histogram: Excess cumulative returns
*)

/// Build cumulative return histogram
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

returnsHist allTranscripts |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Naive Bayes Module

- [Source: Mathias Brandewinder (Github)](https://github.com/mathias-brandewinder/machine-learning-projects-for-dot-net-developers/tree/master/chapter-2/SpamOrHam) 
*)

type Token = string
type Tokenizer = string -> Token Set
type TokenizedDoc = Token Set

type DocGroup = 
    { Proportion : float
      TokenFrequencies : Map<Token, float> }

/// Scoring a document
let tokenScore (group: DocGroup) (token: Token) =
    if group.TokenFrequencies.ContainsKey token
    then log group.TokenFrequencies.[token]
    else 0.0

let score (document: TokenizedDoc) (group: DocGroup) = 
    let scoreToken = tokenScore group
    log group.Proportion + 
    (document |> Seq.sumBy scoreToken)

/// Predicting a label of a document
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

/// Analyzing a group of documents
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

/// Learning from documents
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
## Training and evaluation sets
*)

let training, validation =
    let posOrNeg = 
        allTranscripts
        |> Seq.filter (fun xs -> xs.Label <> Neutral)
        |> Seq.map (fun xs -> (xs.Label, xs.EarningsCall))
        |> Seq.toArray

    let cutoff = (float posOrNeg.Length) * 0.8
    
    posOrNeg.[.. int cutoff], posOrNeg.[int cutoff + 1 ..]

training.Length
validation.Length

(**
## Text processing: tokenization
*)

(**
**Tokenization** is a step which splits longer strings of text into smaller
pieces, or **tokens**. Large chunks of text can be tokenized into sentences,
sentences can be tokenized into words, etc.

Such tokens are then used to compute a documents score as seen in the `score` function ...
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
## Feature engineering: N-Grams
*)

(**
The most common tokenization process is whitespace/unigram tokenization
*)

let nGrams (n: int) (text: string): string [] = 
    let findWords words =

        let isWord word = 
            let candidateMatch = onlyWords.Match word
            if candidateMatch.Success then Some candidateMatch 
            else None

        words
        |> Seq.choose isWord
        |> Seq.map (fun m -> m.Value)

    let tryNGram words = 
        if (words |> Seq.length = n) then Some (words |> String.concat(" "))
        else None

    text.Split(" ")
    |> Seq.windowed n
    |> Seq.map findWords
    |> Seq.choose tryNGram
    |> Seq.toArray

let twoGramsTokenizer (text: string): Set<string> = 
    text
    |> nGrams 2
    |> Set

/// Some tests
let woodpecker = "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."
twoGramsTokenizer woodpecker

(**
## Term Frequency (TF)
$tf_{t,d} = \frac{n_{t,d}}{number of terms in a document}$
$n_{t,d}$ : number of times a term *t* is present in a document *d*.$
*)

type DocTransfromer = string -> string []

type Stat = 
    | TermFreq of float
    | InvDocFreq of float
    | TermFreqInvDocFreq of float

type TermStat = 
    { Term: string 
      Stat: Stat } 

let tf (docTransformer: DocTransfromer) (doc: string): TermStat [] =
    let terms = docTransformer doc
    let nTerms = float terms.Length

    terms
    |> Seq.countBy id
    |> Seq.map (fun (term, count) ->
        let tf = TermFreq ((float count) / (nTerms))
        {Term = term; Stat = tf})
    |> Seq.toArray

(**
## Inverse document frequency (IDF)
$IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})$
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
        let idf = InvDocFreq (log (float n / float docsWithTerm))
        term, { Term = term
                Stat = idf})
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
            | TermFreq tf, InvDocFreq idf -> 
                let term = termIdf.Term
                let tfIdf = TermFreqInvDocFreq (tf * idf)
                Some { Term = term 
                       Stat =  tfIdf }
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

let wordBarChart (words : TermStat []) (title: string) = 
    words
    |> Seq.map (fun xs -> 
        match xs.Term, xs.Stat with 
        | term, TermFreqInvDocFreq tfIdf when tfIdf > 0.-> 
            Some(term, tfIdf)
        | _ -> None)
    |> Seq.choose id
    |> Seq.toArray
    |> Chart.Column
    |> Chart.withTitle title
    |> Chart.withSize (1250., 500.)

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
        | TermFreqInvDocFreq tfIdf -> Some tfIdf
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
            | TermFreqInvDocFreq tfIdf when tfIdf > tfIdfThresh -> Some term.Term
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