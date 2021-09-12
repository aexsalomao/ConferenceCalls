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
### Preprocessing paragraphs pipeline
*)

#load "TextPreprocessing.fsx"
open Preprocessing.Normalization
open Preprocessing.Tokenization
open Preprocessing.NltkData

let preprocessParagraph (paragraph : string) = 
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

let generateBow (call : EarningsCall) = 
    call.Transcript
    |> Seq.collect preprocessParagraph
    // Bag-of-words representation
    |> Seq.countBy id
    |> Seq.toArray

myEars
|> Seq.tryFind (fun xs -> xs.EarningsCall.CallId.Ticker = "TSLA")
|> Option.map (fun xs -> generateBow xs.EarningsCall)

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

let generateFeatureAndLabel (thresh : float) 
                            (ear : EarningsAnnouncementReturn): (Label * BagOfWords) option = 
    match ear.Ear with
    // Filter for significant ears
    | Some earVal when abs earVal >= thresh -> 
        Some (makeLabel earVal thresh, 
              generateBow ear.EarningsCall)
    | _ -> None

(**
Splitting Data: Train and Test
*)

let train, test = 
    let thresh = 0.05
    let generateData, splitData = 
        // Label call and generate Bag-of-Words
        generateFeatureAndLabel thresh,
        // Split dataset (Train)
        getTrainTest 0.9
    
    myEars
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
        let prior =  (float labelsFreqs.Length)/(float n)
        label, prior)
    |> Map

(**
### Vocabulary
*)

let vocabulary = 
    train
    |> Array.collect (fun (_, bow) -> 
        bow
        |> Array.map fst)
    |> Array.distinct

vocabulary.Length

(**
### Bag-of-Words by label (laplace corrected)
*)

let bowByLabel: Map<Label, BagOfWords> = 
    
    // Add label token counts
    let generateLabelBow (xs : (Label * (TokenCount) []) []) = 
        xs 
        |> Seq.collect snd
        |> Seq.groupBy fst
        |> Seq.map (fun (token, tokenCount) -> 
            let totalCount = tokenCount |> Seq.sumBy snd
            token, totalCount)
        |> Map
    
    // Laplace correction
    let fillMissingVocab (labelBow: Map<Token , Count>) = 
        vocabulary
        |> Array.choose (fun vocabToken ->
            match labelBow.TryFind vocabToken with
            | Some count -> Some (vocabToken, count + 1)
            | None -> Some (vocabToken, 1))

    train
    |> Array.groupBy fst
    |> Array.map (fun (label, xs) -> 
            let bow = 
                generateLabelBow xs
                |> fillMissingVocab
            label, bow)
    |> Map

(**
### Label counts
*)
        
let labels: Label [] = 
    train
    |> Seq.distinctBy fst
    |> Seq.map fst
    |> Seq.toArray

let countsByLabel = 
    labels
    |> Seq.map (fun label -> 
        label, bowByLabel.[label] |> Seq.sumBy snd)
    |> Map

(**
### Likelihoods by Label
*)

let computeLikelihoods (label: Label) 
                       (bow : BagOfWords) : (Token * Likelihood) [] = 
    bow
    |> Seq.map (fun (token, count) -> 
        let tokenLikihood = 
            (float count) / (float countsByLabel.[label])
        token, tokenLikihood)
    |> Seq.toArray

let tokenLikelihoods (label : Label): (Label * Map<Token, Likelihood>) =
    bowByLabel.[label]
    |> computeLikelihoods label
    |> Map
    |> fun xs -> (label, xs)

let likelihoodsByLabel: Map<Label, Map<Token, Likelihood>> = 
    labels
    |> Seq.map tokenLikelihoods
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
    let token, count = tokenCount

    match likelihoods.TryFind token with
    | Some l -> 
        l ** float (count)
        |> log 
    | None -> 
        // Laplace correctioncount
        (1./float (totalCount + 1)) ** float (count)
        |> log

let scoreTokenByLabel label = 
    let labelLikelihoods, labelCounts = 
        likelihoodsByLabel.[label], countsByLabel.[label]

    tokenScore labelLikelihoods labelCounts

(**
### Scoring Bag-of-Words
*)

let score (bow : BagOfWords) 
          (label : Label): Label * Prior= 
    bow
    |> Seq.map (scoreTokenByLabel label)
    |> Seq.fold (+) (log labelPriors.[label])
    |> fun score -> label, score

let classify (bow : BagOfWords): Label = 
    let bowToScore = score bow
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
        if classify bow = target then 1. 
        else 0.)

evaluate train
evaluate test