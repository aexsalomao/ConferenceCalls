(**
---
title: Classifying Earnings Calls with Naive Bayes
category: Scripts
categoryindex: 3
index: 3
---
*)

(**
# Classifying Earnings Calls with Naive Bayes
*)

(**
After dowloading earnings transcripts from Motley Fool, we proceeded to compute 
the Earnings Announcement Return (EAR) of each company's earnings announcement 
in `EarningsAnnouncementReturn.fsx`. 

We can use EAR of each call as a *proxy* that is meant to measure the market's 
overall sentiment towards a given earnings call. While a high EAR would indicate 
that the overall market's sentiment was positive, a low EAR would 
indicate precicely the opposite. 

There are many machine learning algorithms to choose from when trying to solve 
a binary or multi-classification problem. Due to its simplicity and intuitive framework, 
a Naive Bayes classifier is often a good place to start.
*)

(**
## Import packages and load scripts
*)

open System
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: FSharp.Stats"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#load "Types.fsx"

open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open Types
open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
## Read transcripts from json file
*)

let readEarJson (jsonFile : string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<array<EarningsAnnouncementReturn>>(json)

let myEars = 
    [
        "data-cache/EarningsAnnouncementReturn2018.json"
        "data-cache/EarningsAnnouncementReturn2019.json"
        "data-cache/EarningsAnnouncementReturn2020.json"
        "data-cache/EarningsAnnouncementReturn2021.json"
    ]
    |> Seq.collect readEarJson
    |> Seq.sortBy (fun xs -> xs.EarningsCall.CallId.Date)
    |> Seq.toArray

myEars.Length
(*** include-it ***)

(**
### Data visualization: Earnings Announcement Returns
*)

let earsHist (ears : float []) (thresh: float) = 

    let obsToPlot name threshExpr =         
        ears 
        |> Array.filter threshExpr
        |> fun filteredEars ->
            let pct = 
                float filteredEars.Length/ float ears.Length 
                |> fun xs -> Math.Round(xs * 100., 2)
            filteredEars
            |> Chart.Histogram
            |> Chart.withTraceName ($"{name} ({pct}%%)")

    [
        obsToPlot "Negative" (fun ret -> ret <= -thresh)
        obsToPlot "Neutral" (fun ret -> abs ret < thresh)
        obsToPlot "Positive" (fun ret -> ret >= thresh)
    ]
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
earsHist earsToPlot 0.05 |> Chart.Show 
(*** hide ***)
earsHist earsToPlot 0.05 |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Preprocessing Text
*)

(**
Before jumping into any sort of text mining technique or analytics, we must first 
convert our unstructered dataset.
*)

(**
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
### Preprocessing paragraphs pipeline
*)

#load "TextPreprocessing.fsx"
open Preprocessing.Normalization
open Preprocessing.Tokenization
open Preprocessing.NltkData

let preprocessParagraph (paragraph : string) = 
    // Scope of text
    if paragraph.Length > 100 then
        paragraph
        // Detect sentence boundaries
        |> splitParagraph
        |> Array.collect (fun phrase ->
            phrase
            // Normalize
            |> getOnlyWords
            |> expandContractions
            // Tokenize
            |> nGrams 1
            // Stop words removal
            |> Array.choose removeStopWords
            // Empty string removal
            |> Array.filter (fun xs -> xs.Length <> 0))
        |> Some
    else None

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
|> Array.choose preprocessParagraph
|> Array.concat
// Bag-of-words representation
|> Array.countBy id

(**
### Splitting Data: Train and Test sets
*)

let makeLabel earVal thresh = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let getTrainTest cutoffPct ears = 
    let rnd = System.Random(42)
    ears
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.toArray
    |> fun xs -> 
        let cutoff = float xs.Length * cutoffPct
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]

let generateFeatureAndLabel (thresh : float) (ear : EarningsAnnouncementReturn): (Label * BagOfWords) option = 
    match ear.Ear with
    | Some earVal -> 
        let bow = 
            ear.EarningsCall.Transcript
            // Text-preprocessing
            |> Array.Parallel.choose preprocessParagraph
            |> Array.concat
            // Bag-of-words
            |> Array.countBy id

        Some (makeLabel earVal thresh, bow)
    | None -> None

let filterByEar thresh ear = 
    match ear.Ear with
    | Some earVal when abs earVal >= thresh -> Some ear
    | _ -> None

(**
Splitting Data: Train and Test
*)

let train, test = 
    let thresh = 0.05
    let significantEars, generateData, splitData = 
        // Only keep obs with high abs (Ears)
        filterByEar thresh, 
        // Label obs and create Bag-of-Words
        generateFeatureAndLabel thresh,
        // Split dataset (0.9 -> Train)
        getTrainTest 0.9
    
    myEars
    |> Seq.choose significantEars
    |> Seq.choose generateData
    |> splitData

train.Length
test.Length

(**
### Multinomial Naive Bayes: Bag of words approach
*)

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
    let tokenCounts (xs : (Label * (TokenCount) array) array) = 
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

(**
### Token Likelihoods by Label
*)

let labels: Label [] = 
    train
    |> Seq.distinctBy fst
    |> Seq.map fst
    |> Seq.toArray

let computeLikelihoods (tokenCounts : TokenCount []): (Token * Likelihood) [] = 
    let totalCount = 
        tokenCounts 
        |> Seq.sumBy snd
    tokenCounts
    |> Seq.map (fun (token, freq) -> 
        let tokenLikihood = (float freq / float totalCount)
        token, tokenLikihood)
    |> Seq.toArray

let tokenLikelihoods (label : Label): (Label * Map<Token, Likelihood>) option =
    match bowByLabel.TryFind label with
    | Some bow -> 
        bow
        |> computeLikelihoods
        |> Map
        |> fun xs -> Some (label, xs)
    | None -> None

let likelihoodsByLabel: Map<Label, Map<Token, Likelihood>> = 
    labels
    |> Seq.choose tokenLikelihoods
    |> Seq.toArray
    |> Map

(**
### Token score
*)

(**
Take logs to prevent underflow
*)

let tokenScore (likelihoods : Map<Token, Likelihood>) 
               (totalCount : Count)
               (tokenCount : Token * Count): TokenScore = 

    match likelihoods.TryFind (fst tokenCount) with
    | Some l -> 
        l ** float (snd tokenCount)
        |> log 
    | None -> 
        (1./float totalCount) ** float (snd tokenCount)
        |> log

let scoreTokenByLabel label = 
    let labelLikelihoods, labelBow = 
        likelihoodsByLabel.[label], bowByLabel.[label]

    tokenScore labelLikelihoods (labelBow |> Seq.sumBy snd)

(**
### Scoring Bag-of-Words
*)

let scoreBowByLabel (bow: BagOfWords) (label: Label): Label * Prior= 
    bow
    |> Seq.map (scoreTokenByLabel label)
    |> Seq.fold (+) (log labelPriors.[label])
    |> fun score -> label, score

let classifyBow (bow : BagOfWords): Label = 
    let bowToScore = scoreBowByLabel bow
    labels
    |> Seq.map bowToScore
    |> Seq.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (labelledBoW : (Label * BagOfWords) []): float =
    labelledBoW
    |> Seq.averageBy (fun (target, bow) ->  
        if classifyBow bow = target then 1. 
        else 0.)

evaluate train
evaluate test