(**
---
title: Classifying earnings call transcripts with Naive Bayes
category: Scripts
categoryindex: 3
index: 3
---
*)

(**
# Classifying earnings call transcripts with Naive Bayes
*)

(**
After dowloading earnings transcripts from Motley Fool, we proceeded to compute 
the EAR of each respective company around its earnings call in `EarningsAnnouncementReturn.fsx`. 
We can now use the EAR of each call as a *proxy* that is meant to measure the market's 
overall sentiment towards a given earnings call.

There are many machine learning algorithms to choose from when trying to solve a binary or multi-classification problem.
Due to its simplicity and intuitive framework, a Naive Bayes classifier is often a good place to start.
*)

(**
## Import packages and load scripts
*)

#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: FSharp.Stats"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#load "Types.fsx"

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open Types

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

(**
## Read transcripts from json file
*)

let readEarJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<array<EarningsAnnouncementReturn>>(json)

let myEars = 
    [|
    "data-cache/EarningsAnnouncementReturn2018.json"
    "data-cache/EarningsAnnouncementReturn2019.json"
    "data-cache/EarningsAnnouncementReturn2020.json"
    "data-cache/EarningsAnnouncementReturn2021.json"
    |]
    |> Array.collect readEarJson
    |> Array.sortBy (fun xs -> xs.EarningsCall.CallId.Date)

myEars.Length

(**
### Data visualization: Earnings Announcement Returns
*)

let earsHist (ears: float [])= 
    let thresh = 0.05
    let negThresh ret = ret <= -thresh
    let neuThresh ret = abs ret < thresh
    let posThresh ret = ret >= thresh

    let histObs thresh name = 
        ears 
        |> Array.filter thresh
        |> fun filteredEars ->
            let pct = 
                float filteredEars.Length/ float ears.Length 
                |> fun xs -> Math.Round(xs * 100., 2)
            filteredEars
            |> Chart.Histogram
            |> Chart.withTraceName ($"{name} ({pct}%%)")
    [|
    histObs negThresh "Negative"
    histObs neuThresh "Neutral"
    histObs posThresh "Positive"
    |]
    |> Chart.Combine
    |> Chart.withTitle ("Earnings Announcement Returns (EAR)")
    |> Chart.withX_AxisStyle ("EAR")
    |> Chart.withY_AxisStyle ("Count")
    |> Chart.withSize (1000., 500.)

let earsToPlot = 
    myEars
    |> Array.choose (fun xs -> xs.Ear)
    // Remove outliers ...
    |> Array.filter (fun xs -> abs xs < 0.5)

(*** do-not-eval ***)
earsHist earsToPlot |> Chart.Show 
(*** hide ***)
earsHist earsToPlot |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Preprocessing Text
*)

(**
Before jumping into any sort of text mining technique or analytics, we must first 
transform our dataset of earnings calls.

Basic steps:
1. Choose the scope of text to be processed
2. Tokenize
3. Remove stop words
4. Stem
5. Normalize spelling
6. Detect sentence boundaries
7. Normalize case
*)

(**
### Scope of text
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
    myEars
    |> Array.collect (fun xs -> 
        xs.EarningsCall.Transcript
        |> Array.filter (fun paragraph -> paragraph.Length < 100))

shortParagraphs
|> Array.take 5
|> Array.iter (printfn "%s")
(*** include-output***)

(**
### Preprocessing paragraphs
*)

#load "TextPreprocessing.fsx"
open Preprocessing.Normalization
open Preprocessing.Tokenization
open Preprocessing.NltkData

let preprocessParagraph (paragraph: string) = 
        paragraph
        |> splitParagraph
        |> Array.collect (fun phrase ->
            phrase
            |> getOnlyWords
            |> expandContractions
            // Tokenize
            |> nGrams 1
            // Stop words removal
            |> Array.choose removeStopWords
            // Empty string removal
            |> Array.filter (fun xs -> xs.Length <> 0))

let sample = 
    [|
    "Elon Musk -- Chief Executive Officer"
    
    "Sure. So to recap, Q2 2021 was a record quarter on many levels. 
    We achieved record production, deliveries, and surpassed over $1 billion 
    in GAAP net income for the first time in Tesla's history. I'd really like 
    to congratulate everyone in Tesla for the amazing job."

    "This is really an incredible milestone. It also seems that public sentiment 
    toward EVs is at an inflection point. And at this point, I think almost everyone 
    agrees that electric vehicles are the only way forward. Regarding supply chain, 
    while we're making cars at full speed, the global chip shortage situation remains 
    quite serious."
    |]

sample
|> Array.collect preprocessParagraph
// Bag-of-words representation
|> Array.countBy id

(**
### Splitting Data: Train and Test sets
*)

let labelSentiment earVal thresh = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let trainValid cutoffPct ears = 
    let rnd = System.Random(42)
    ears
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.toArray
    |> fun xs -> 
        let cutoff = float xs.Length * cutoffPct
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]

let generateFeatureAndLabel (thresh: float) (ear: EarningsAnnouncementReturn) = 
    match ear.Ear with
    | Some earVal -> 
        let processedText = 
            ear.EarningsCall.Transcript
            // Keep only long paragraphs (remove opening remarks)
            |> Array.filter (fun xs -> xs.Length > 100)
            // Text-preprocessing
            |> Array.Parallel.collect preprocessParagraph
            |> Array.countBy id

        Some (labelSentiment earVal thresh, 
              processedText)
    | None -> None

let earAboveThresh thresh ear = 
    ear.Ear
    |> function
    | Some earVal when abs earVal >= thresh -> Some ear
    | _ -> None

(**
Splitting Data: Train and Test
*)

let train, test = 
    let thresh = 0.05
    let significantEars, generateData, splitData = 
        earAboveThresh thresh, 
        generateFeatureAndLabel thresh,
        trainValid 0.9

    myEars
    |> Seq.choose significantEars
    |> Seq.choose generateData
    |> splitData

train.Length
test.Length

(**
### Naive Bayes: Bag of words approach
*)

(**
### Types
*)

type Label = Sentiment
type Prior = float
type Token = string
type Count = int
type Likelihood = float

type TokenCount = Token * Count
type BagOfWords = TokenCount []

type TokenScore = float
type DocScore = float


(**
### Priors
*)

let labelPriors: Map<Label, Prior>= 
    let n = train.Length
    train
    |> Array.groupBy fst
    |> Array.map (fun (label, labelsFreqs) -> 
        let labelPrior =  (float labelsFreqs.Length)/(float n)
        label, labelPrior)
    |> Map

labelPriors

(**
### Bag-of-Words by Label, Token counts
*)

let bowByLabel: Map<Label, BagOfWords> = 
    let tokenCounts (xs: (Label * (TokenCount) array) array) = 
        xs 
        |> Seq.collect snd
        |> Seq.groupBy fst
        |> Seq.map (fun (token, tokenCount) -> 
            let totalCount = tokenCount |> Seq.sumBy snd
            token, totalCount)
        |> Seq.toArray

    train
    |> Array.groupBy fst
    |> Array.map (fun (label, xs) -> 
        label, tokenCounts xs)
    |> Map

let tokenCountsByLabel: Map<Label, Count>= 
    let computeTotalCounts label = 
        label, 
        bowByLabel.[label] |> Seq.sumBy snd

    train
    |> Seq.map fst
    |> Seq.distinct
    |> Seq.map computeTotalCounts
    |> Map

(**
### Token Likelihoods by Label
*)

let labels: Label [] = 
    train
    |> Seq.distinctBy fst
    |> Seq.map fst
    |> Seq.toArray

let computeLikelihoods (tokenCounts: TokenCount []): (Token * Likelihood) [] = 
    let totalCount = 
        tokenCounts 
        |> Seq.sumBy snd
    tokenCounts
    |> Seq.map (fun (token, freq) -> 
        let tokenLikihood = (float freq / float totalCount)
        token, tokenLikihood)
    |> Seq.toArray

let likelihood (label: Label): Label * Map<Token, Likelihood> =
    bowByLabel.[label]
    |> computeLikelihoods
    |> Map
    |> fun xs -> label, xs

let likelihoodsByLabel: Map<Label, Map<Token, Likelihood>> = 
    labels
    |> Seq.map likelihood
    |> Seq.toArray
    |> Map

(**
### Token score
*)

let tokenScore (likelihoods: Map<Token, Likelihood>) 
               (totalCount: Count)
               (tokenCount: Token * Count): TokenScore = 

    match likelihoods.TryFind(fst tokenCount) with
    | Some l -> 
        l ** float (snd tokenCount)
        |> log 
    | None -> 
        (1./float totalCount) ** float (snd tokenCount)
        |> log

let scoreTokenByLabel label = 
    tokenScore likelihoodsByLabel.[label] tokenCountsByLabel.[label]

(**
### Scoring Bag-of-Words
*)

let scoreBowByLabel (bow: BagOfWords) (label: Label): Label * DocScore = 
    bow
    |> Seq.map (scoreTokenByLabel label)
    |> Seq.fold (+) (log labelPriors.[label])
    |> fun score -> label, score

let classifyBow (bow: BagOfWords): Label = 
    let bowToScore = scoreBowByLabel bow
    labels
    |> Seq.map bowToScore
    |> Seq.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (labelledBoW: (Label * BagOfWords)[]): float =
    labelledBoW
    |> Seq.averageBy (fun (target, bow) ->  
        if classifyBow bow = target then 1. 
        else 0.)

evaluate (train |> Array.take 10000)
evaluate test