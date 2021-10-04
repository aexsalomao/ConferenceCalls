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

We can use the EAR of each call as a *proxy* that is meant to measure the market's 
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
open System.IO
open FSharp.Data
Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

#r "nuget: FSharp.Stats"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

#r "nuget: FSharp.Collections.ParallelSeq, 1.1.4"
#load "Types.fsx"
#load "TextPreprocessing.fsx"

open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats
open FSharp.Collections.ParallelSeq
open System.Text.RegularExpressions

open Types
open Preprocessing.Normalization
open Preprocessing.Tokenization
open Preprocessing.NltkData

(**
## IMBD dataset
*)

let readFile path =
    let text = File.ReadAllText(path)
    text.Split(' ')    

let readTheFiles path =
    let files = Directory.GetFiles(path)
    files |> Array.map readFile

let posReviewsTrain : (string * Class) []  = 
    readTheFiles "data-cache/ImdbTrain/pos"
    |> Array.map (fun review -> (review |> String.concat(" ")), Positive)

let negReviewsTrain : (string * Class) [] = 
    readTheFiles "data-cache/ImdbTrain/neg"
    |> Array.map (fun review -> (review |> String.concat(" ")), Negative)

let imbdTrain = 
    [|posReviewsTrain; negReviewsTrain|]
    |> Array.concat

let posReviewsTest : (string * Class) []  = 
    readTheFiles "data-cache/ImdbTest/pos"
    |> Array.map (fun review -> (review |> String.concat(" ")), Positive)

let negReviewsTest : (string * Class) [] = 
    readTheFiles "data-cache/ImdbTest/neg"
    |> Array.map (fun review -> (review |> String.concat(" ")), Negative)

let imbdTest = 
    [|posReviewsTest; negReviewsTest|]
    |> Array.concat

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
    ]
    |> Seq.collect readEarJson
    |> Seq.choose (fun sample -> 
        match sample.Ear with
        | Some _ -> Some sample
        | None -> None)
    |> Seq.sortBy (fun xs -> xs.EarningsCall.CallId.Date)
    |> Seq.toArray

myEars.Length
(*** include-it ***)

(**
### Data visualization: Earnings Announcement Returns
*)

let earsHist (ears : float array) 
             (thresh: float) = 

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
/// earsHist earsToPlot 0.05 |> Chart.Show 
(*** hide ***)
/// earsHist earsToPlot 0.05 |> GenericChart.toChartHTML
(*** include-it-raw ***)

(**
## Generate Dataset
*)

let labelEar (earVal : float) thresh : Class = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let trainRaw, testRaw = 
    myEars
    |> Array.choose (fun xs -> 
        match xs.Ear with
        | Some ear -> 
            let document = xs.EarningsCall.Transcript |> String.concat(" ")
            let label : Class = labelEar ear 0.05
            if label <> Neutral then Some (document, label)
            else None
        | None -> None)
    |> fun xs -> 
        let cutoff = float xs.Length * 0.8
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]

trainRaw.Length
testRaw.Length

(**
## AdHoc blacklists (Feature Engineering): 
1. Identifying proper nouns with NLTK database
2. Fetching company names using requests
*)

/// Names
let nltkNames = 
    System.IO.File.ReadLines("data-cache\NamesNLTK.txt")
    |> Seq.map (fun xs -> xs.ToLowerInvariant())
    |> Set

(**
## Backlist 2: Identifying company names using ticker symbols
*)

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
## Tokenization
*)

(**
## Tokenizer
*)

(**
### Tokenize documents
*)

// Tokenize all documents
let tokenizeDocument (nGram : int)
                     (rawDocument : string) 
                     : Token [] = 
    rawDocument.ToLowerInvariant().Split(" ")
    // Blacklist filter
    |> Array.filter (nltkNames.Contains >> not)
    |> String.concat(" ")
    // Normalize
    |> getOnlyWords
    |> expandContractions
    // Tokenize
    |> nGrams nGram
    // Stop words removal
    |> Array.choose removeStopWords
    // Empty string removal
    |> Array.filter (fun xs -> xs.Length <> 0)

let generateTokenizedDocuments (tokenizer : Tokenizer)
                               (labelledRawDocuments : (string * Class) []) 
                               : LabelledDocument [] = 
    labelledRawDocuments
    |> Array.Parallel.map (fun (doc, label) -> 
        tokenizer doc, label)

(**
### Bag of words representation
*)

// Top Tokens from sample
let getTopNTokens (sampleMaxTokens : int)
                  (labelledDocuments : LabelledDocument []) 
                  : Set<Token> = 
    labelledDocuments
    |> Array.collect fst
    |> Array.countBy id
    |> Array.sortByDescending snd
    |> Array.truncate sampleMaxTokens
    |> Array.map fst
    |> Set

// Generate bag of words using only top tokens
let getTopTokenBow (topTokens : Set<Token>)
                   (document : Document) 
                   : BagOfWords = 
    document
    |> Array.countBy id
    |> Array.filter (fun (token, _) -> topTokens.Contains token)
    |> Array.sortByDescending snd

let generateTopTokenBows (topTokens : Set<Token>)
                         (labelledDocuments : LabelledDocument []) 
                         : LabelledBagOfWords [] =
    labelledDocuments
    |> Array.Parallel.map (fun (tokenizedDoc, label) -> 
        getTopTokenBow topTokens tokenizedDoc, label)

(**
### Preprocess text
*)

type TextPreprocessor = 
    { NGram : int 
      MaxSampleTokens : int}

let preprocessText (textPreprocessor : TextPreprocessor)
                   (rawDocumentsTrain : (string * Class) []) 
                   (rawDocumentsTest : (string * Class) [])
                   : LabelledBagOfWords [] * LabelledBagOfWords [] = 

    // Tokenize documents (nGrams)
    let nGramTokenizer = 
        tokenizeDocument textPreprocessor.NGram

    let tokenizedTrainRaw, tokenizedTestRaw = 
        generateTokenizedDocuments nGramTokenizer rawDocumentsTrain,
        generateTokenizedDocuments nGramTokenizer rawDocumentsTest
   
    // Generate bag of words using most frequent tokens
    let topTokens = 
        getTopNTokens textPreprocessor.MaxSampleTokens tokenizedTrainRaw
    
    generateTopTokenBows topTokens tokenizedTrainRaw,
    generateTopTokenBows topTokens tokenizedTestRaw

(**
## Training the Naive Bayes classifier
*)

(**
#### Vocabulary
*)

// Corpus vocabulary from Bow
let getCorpusVocab (labelledBows : LabelledBagOfWords []) 
                   : Set<Token> = 
    labelledBows
    |> Array.collect (fun (bow, _) -> 
        bow
        |> Array.map fst)
    |> Set

(**
#### Class Priors
*)

let getPriors (labelledBows : LabelledBagOfWords []) 
              : Map<Class, Prior> = 
    
    let n = labelledBows.Length
    
    labelledBows
    |> Array.groupBy snd
    |> Array.map (fun (label, labelFreqs) -> 
        let prior =  (float (labelFreqs.Length)/(float n))
        label, prior)
    |> Map

(**
#### Aggregate Token Counts by Class -> Class Bag of Words
*)

let getClassBagofWords (labelledBow : LabelledBagOfWords [])
                       : (Class * Token * Count) [] = 
    labelledBow
    |> Array.groupBy snd
    |> Array.collect (fun (c, classTokenCounts) -> 
        classTokenCounts
        |> Array.filter (fun (_, label) -> label=c)
        |> Array.collect fst
        |> Array.groupBy fst
        |> Array.map (fun (token, tokenCounts) -> c, token, Array.sumBy snd tokenCounts))

(**
#### Token Likelihoods by Class
*)

let getTokenLikelihoods (labelledBows : LabelledBagOfWords [])
                        : Map<Class, Map<Token, Likelihood>> = 
    let vocabN = 
        getCorpusVocab labelledBows |> Seq.length

    getClassBagofWords labelledBows
    |> Array.groupBy (fun (_, token, _) -> token)
    |> Array.collect (fun (_, xs) -> 
        let totalCounts = 
            xs
            |> Array.sumBy (fun (_, _, counts) -> counts)
        xs
        |> Array.map (fun (c, token, counts) -> 
            let tokenLikelihood = 
                float (counts + 1) / float (totalCounts + vocabN)
            (c, token, tokenLikelihood)))
    |> Array.groupBy (fun (c, _, _) -> c)
    |> Array.map (fun (c, xs) -> 
        c, 
        xs
        |> Array.map (fun (_, token, counts) -> token, counts)
        |> Map)
    |> Map

(**
#### Building the Naive Bayes Classifier
*)

type NbClassifierInfo = 
    { Priors : Map<Class, Prior>
      Likelihoods : Map<Class, Map<Token, Likelihood>>}

let buildNbClassifier (labelledBows : LabelledBagOfWords []) = 
    { Priors = getPriors labelledBows
      Likelihoods = getTokenLikelihoods labelledBows}

(**
## Classifying new Documents
*)

let classifyBagOfWords (nbClassifierInfo : NbClassifierInfo)
                       (bow : BagOfWords)
                       : Class =
    nbClassifierInfo.Priors
    |> Map.toArray
    |> Array.choose (fun (c, prior) -> 
        match nbClassifierInfo.Likelihoods.TryFind c with
        | Some tokenLikelihoods ->
            bow
            |> Array.choose (fun (token, count) -> 
                match tokenLikelihoods.TryFind token with
                | Some likelihood -> 
                    let tokenScore = log (likelihood ** float count)
                    Some tokenScore
                | None -> None)
            |> Array.fold (+) (log prior)
            |> fun docScore -> Some (c, docScore)
        | None -> None)   
    |> Array.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (nbClassifier : NbClassifier)
             (labelledBows : LabelledBagOfWords [])
             : Accuracy =
   
    labelledBows
    |> PSeq.averageBy (fun (bow, label) ->  
        if nbClassifier bow = label then 1. 
        else 0.)

(**
### Model 1
*)

let tp1 = {NGram=1; MaxSampleTokens=10000}
let trainBow, testBow = preprocessText tp1 imbdTrain imbdTest

let myNbClassifier : NbClassifier = 
    buildNbClassifier trainBow
    |> classifyBagOfWords

let trainEval = evaluate myNbClassifier trainBow
let testEval = evaluate myNbClassifier testBow