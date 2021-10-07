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
#### AdHoc blacklists : 
1. Identifying proper nouns with NLTK database
2. Fetching company names using requests
*)

/// Names
let nltkNames = 
    System.IO.File.ReadLines("data-cache\NamesNLTK.txt")
    |> Seq.map (fun xs -> xs.ToLowerInvariant())
    |> Set

(**
## Preprocessing Text
*)

(**
#### Tokenize documents
*)

// Tokenize all documents
let tokenizeDocumentWith (nGram : int)
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

let tokenizeDocuments (tokenizer : Tokenizer)
                      (labelledRawDocuments : (string * Class) []) 
                      : LabelledDocument [] = 
    labelledRawDocuments
    |> Array.Parallel.map (fun (doc, label) -> 
        tokenizer doc, label)

(**
#### Bag of words representation
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
    |> Array.Parallel.map (fun (doc, label) -> 
        getTopTokenBow topTokens doc, label)

(**
#### Preprocess text
*)

type TextVectorizer = 
    { NGram : int 
      MaxSampleTokens : int}

let vectorizeTrainTest (textPreprocessor : TextVectorizer)
                       (rawDocumentsTrain : (string * Class) []) 
                       (rawDocumentsTest : (string * Class) [])
                       : LabelledBagOfWords [] * LabelledBagOfWords [] = 

    // Tokenize documents (nGrams)
    let tokenizer = 
        tokenizeDocumentWith textPreprocessor.NGram

    let tokenizedTrain, tokenizedTest = 
        tokenizeDocuments tokenizer rawDocumentsTrain,
        tokenizeDocuments tokenizer rawDocumentsTest
   
    // Generate bag of words using most frequent tokens
    let topTokens = 
        getTopNTokens textPreprocessor.MaxSampleTokens tokenizedTrain
    
    generateTopTokenBows topTokens tokenizedTrain,
    generateTopTokenBows topTokens tokenizedTest

(**
## Training the Naive Bayes classifier
*)

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

let getClassBagofWords 
    (labelledBow : LabelledBagOfWords [])
    : (Class * Token * Count) [] = 
    
    labelledBow
    |> Array.groupBy snd
    |> Array.collect (fun (c, classBagOfWords) -> 
        classBagOfWords
        |> Array.filter (fun (_, label) -> label=c)
        |> Array.collect fst
        |> Array.groupBy fst
        |> Array.map (fun (token, tokenCounts) -> 
            c, token, Array.sumBy snd tokenCounts))

(**
#### Token Likelihoods by Class
*)

let computeTokenLikilihoods 
    (classBagOfWords : (Class * Token * Count) [])
    (vocabN : Count) 
    : (Class * Token * Likelihood) [] = 
    
    classBagOfWords
    |> Array.groupBy (fun (_, token, _) -> token)
    |> Array.collect (fun (_, xs) -> 
        /// Compute total token counts within all classes
        let totalTokenCounts = 
            xs
            |> Array.sumBy (fun (_, _, counts) -> counts)
        /// Compute token likelihood for all classes (Laplace corrected)
        xs
        |> Array.map (fun (c, token, counts) -> 
            let tokenLikelihood = 
                float (counts + 1) / float (totalTokenCounts + vocabN)
            (c, token, tokenLikelihood)))

let getClassLikelihoodsMap (tokenLikelihoods :  (Class * Token * Likelihood) []) 
                           : Map<Class, Map<Token, Likelihood>> = 
    tokenLikelihoods
    |> Array.groupBy (fun (c, _, _) -> c)
    |> Array.map (fun (c, xs) -> 
        c, 
        xs
        |> Array.map (fun (_, token, counts) -> token, counts)
        |> Map)
    |> Map

let getTokenLikelihoods (labelledBows : LabelledBagOfWords [])
                        : Map<Class, Map<Token, Likelihood>> = 
    
    let classBagOfWords = 
        getClassBagofWords labelledBows

    let vocabN = 
        classBagOfWords
        |> Array.distinctBy (fun (_, token, _) -> token)
        |> Array.length

    computeTokenLikilihoods classBagOfWords vocabN
    |> getClassLikelihoodsMap

(**
#### Building the Naive Bayes Classifier
*)

type NbClassifierInfo = 
    { Priors : Map<Class, Prior>
      Likelihoods : Map<Class, Map<Token, Likelihood>>}

let trainNbClassifier (labelledBows : LabelledBagOfWords []) 
                      : NbClassifierInfo = 
    { Priors = getPriors labelledBows
      Likelihoods = getTokenLikelihoods labelledBows}

(**
## Classifying new Documents
*)

/// Fetch token scores from bag of words
let getTokenScores 
    (tokenLikelihoods : Map<Token, Likelihood>)
    (bow : BagOfWords) 
    : TokenScore [] = 
    
    bow
    |> Array.choose (fun (token, count) -> 
        match tokenLikelihoods.TryFind token with
        | Some likelihood -> 
            Some (log (likelihood ** float count))
        | None -> None)

/// Computes final score by adding token scores, prior
let computeDocumentScore 
    (prior : Prior)
    (tokenScores : TokenScore []) 
    : DocumentScore =
    
    tokenScores
    |> Array.fold (+) (log prior)

/// Computes document scores and classifies document
let classifyBagOfWords 
    (nbClassifierInfo : NbClassifierInfo)
    (bow : BagOfWords)
    : Class =
    
    nbClassifierInfo.Priors
    |> Map.toArray
    |> Array.choose (fun (c, prior) -> 
        match nbClassifierInfo.Likelihoods.TryFind c with
        | Some tokenLikelihoods ->
            bow
            |> getTokenScores tokenLikelihoods  
            |> computeDocumentScore prior
            |> fun docScore -> Some (c, docScore)
        | None -> None)   
    |> Array.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (nbClassifierInfo : NbClassifierInfo)
             (labelledBows : LabelledBagOfWords [])
             : Accuracy =
    
    let classifyBow = classifyBagOfWords nbClassifierInfo
   
    labelledBows
    |> PSeq.averageBy (fun (bow, label) ->  
        if classifyBow bow = label then 1. 
        else 0.)

(**
### Model 1
*)

let tp1 = {NGram=2; MaxSampleTokens=5000}
let trainBow, testBow = vectorizeTrainTest tp1 trainRaw testRaw

let fittedClassifier = trainNbClassifier trainBow

let trainEval = evaluate fittedClassifier trainBow
let testEval = evaluate fittedClassifier testBow