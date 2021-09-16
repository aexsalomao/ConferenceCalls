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

let earsHist 
    (ears : float list) 
    (thresh: float) = 

    let obsToPlot name threshExpr =         
        ears 
        |> List.filter threshExpr
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

let nGramsTokenizer 
    (nGram : int)
    (paragraph : string) = 

    paragraph
    // Detect sentence boundaries
    |> splitParagraph
    |> Seq.collect (fun phrase ->
        phrase
        // Normalize
        |> getOnlyWords
        |> expandContractions
        // Tokenize
        |> nGrams nGram
        // Stop words removal
        |> Seq.choose removeStopWords
        // Empty string removal
        |> Seq.filter (fun xs -> xs.Length <> 0)
        |> Seq.toList)

let makeBow 
    (nGram : int)
    (call : EarningsCall) 
    : BagOfWords =

    call.Transcript
    |> Seq.collect (nGramsTokenizer nGram)
    // Bag-of-words representation
    |> Seq.countBy id
    |> Seq.toList

myEars
|> Seq.tryFind (fun xs -> xs.EarningsCall.CallId.Ticker = "TSLA")
|> Option.map (fun xs -> 
    makeBow 2 xs.EarningsCall 
    |> List.sortByDescending snd
    |> List.take 10)

(**
### Splitting Data: Train and Test sets
*)

let makeLabel (earVal : float) thresh : Sentiment = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let getFeatureAndLabel 
    (nGram : int)
    (thresh : float) 
    (ear : EarningsAnnouncementReturn) 
    : (Label * BagOfWords) option = 
    
    match ear.Ear with
    // Filter for significant ears
    | Some earVal when abs earVal >= thresh -> 
        Some (makeLabel earVal thresh, 
              makeBow nGram ear.EarningsCall)
    | _ -> None

let getTrainTest 
    (splitCuttof : float) 
    (ears : seq<'SampleObs>)
    : 'SampleObs [] * 'SampleObs [] = 
    
    let rnd = Random(0)
    ears
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.toArray
    |> fun xs -> 
        let cutoff = float xs.Length * splitCuttof
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]

(**
### Term Frequency - Inverse Document Frequency
*)

(**
#### Term frequency (Tf)
*)

let tf 
    (bow : BagOfWords) 
    : (Token * Tf) list = 
    
    let docTokenCounts = 
        Seq.sumBy snd bow

    bow
    |> List.map (fun (token, count) -> 
        let tf = (float count)/(float docTokenCounts)
        token, tf)

(**
#### Inverse document frequency (Idf)
*)

let idf 
    (trainSamples : (Label * BagOfWords) []) 
    : (Token * Idf) list = 

    let numDocs = 
        trainSamples.Length

    trainSamples
    |> Seq.collect (fun (_, bow) -> 
        bow
        |> List.map fst)
    |> Seq.countBy id
    |> Seq.map (fun (token, numDocsWithToken) -> 
        let idf = (float numDocs) / (float numDocsWithToken)
        token, log idf)
    |> Seq.toList
  
(**
#### Term frequency - inverse document frequency (TfIdf)
*)

let tfIdf 
    (idf : Map<Token, Idf>) 
    (idfPrior : float)
    (bow : BagOfWords) 
    : (Token * TfIdf) list = 

    tf bow
    |> List.choose (fun (token, tf) -> 
        match idf.TryFind token with
        // Word appeared in train
        | Some idf -> 
            let tfIdf = tf * idf
            Some (token, tfIdf)
        // Word did not appear in train 
        | None -> 
            let tfIdf = tf * idfPrior
            Some (token, tfIdf))

(**
#### Filter Train and Test samples by TfIdf
*)

let filterByTfIdfWith 
    (idf : Map<Token, float>)
    (idfPrior : float)
    (thresh : float)
    (sampleObs : Label * BagOfWords) 
    : Label * BagOfWords = 

    let label, bow = sampleObs

    let filteredBow = 
        tfIdf idf idfPrior bow
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> Seq.take ((thresh * float bow.Length) |> int)
        |> Set
        |> fun highValueTokens -> 
            bow
            |> List.filter (fun (token, _) -> 
                highValueTokens.Contains token)
                
    (label, filteredBow)

let applyTfIdfFilter 
    (topKPctIfIdf : float)
    (train, test) =

    let corpusIdf : Map<Token, Idf> =
        idf train 
        |> Map
    
    let idfPrior : Idf = 
        corpusIdf
        |> Map.toArray
        |> Array.averageBy snd

    let tfIdfFilter = 
        filterByTfIdfWith corpusIdf idfPrior topKPctIfIdf

    Array.Parallel.map tfIdfFilter train,
    Array.Parallel.map tfIdfFilter test

(**
### Multinomial Naive Bayes: Bag of words approach
*)

(**
### Priors
*)

let getPriors 
    (trainSample : (Label * BagOfWords)[])
    : (Label * Prior) list = 
    
    let n = trainSample.Length
    
    trainSample
    |> Seq.groupBy fst
    |> Seq.map (fun (label, labelsFreqs) -> 
        let prior =  (float (labelsFreqs |> Seq.length)/(float n))
        label, prior)
    |> Seq.toList

(**
#### Vocabulary
*)

// Vocabulary from filtered corpus
let getCorpusVocab 
    (trainSamples : (Label * BagOfWords) []) 
    : Set<Token> = 
    
    trainSamples
    |> Seq.collect (fun (_, bow) -> 
        bow
        |> Seq.map fst)
    |> Set

(**
#### Combining samples to form a bag-of-words for each label
*)

// Creates Label BoW
let combineBow 
    (samples : (Label * BagOfWords) []) = 
    samples 
    |> Seq.collect snd
    |> Seq.groupBy fst
    |> Seq.map (fun (token, tokenCounts) -> 
        let totalCount = Seq.sumBy snd tokenCounts
        token, totalCount)

// Laplace correction
let fillMissingVocab 
    (vocab : Set<Token>)
    (labelBow : Map<Token , Count>) 
    : BagOfWords = 
    
    vocab
    |> Set.toList
    |> List.choose (fun vocabToken ->
        match labelBow.TryFind vocabToken with
        | Some count -> Some (vocabToken, count + 1)
        | None -> Some (vocabToken, 1))

let getLabelBows 
    (vocab : Set<Token>)
    (samples : (Label * BagOfWords) [])
    : (Label * BagOfWords) [] = 
    
    samples
    |> Array.groupBy fst
    |> Array.map (fun (label, samples) -> 
        
        let bow = 
            combineBow samples
            |> Map
            |> fun labelBow ->
                fillMissingVocab vocab labelBow    

        label, bow)

(**
### Token Likelihoods by Label
*)

let getLabelLikelihoods 
    (labelBows : Map<Label, BagOfWords>) 
    : (Label * TokenLikelihoods) list =

    labelBows
    |> Map.toList
    |> List.map (fun (label, bow) ->
        let n = Seq.sumBy snd bow
        let tokenLikelihoods = 
            bow
            |> List.map (fun (token, count) -> 
                token, (float count)/(float n))
        label, tokenLikelihoods |> Map)

(**
### Token scores
*)
type NbClassifier =
    { Likelihoods :  Map<Label, TokenLikelihoods>
      Priors : Map<Label, Prior> }

let getTokenScore 
    (tokenLikelihoods : Map<Token, Likelihood>)
    (tokenCount : Token * Count) 
    : TokenScore option = 
    
    let token, count = tokenCount

    match tokenLikelihoods.TryFind token with
    | Some tL -> 
        let tokenScore = log (tL ** (float count))
        Some tokenScore
    | _ -> None

(**
### Document scores
*)

let computeDocScore 
    (label : Label)
    (nbClassifier : NbClassifier)
    (bow : BagOfWords)
    : DocScore = 

    let prior, tokenLikelihoods = 
        nbClassifier.Priors.[label], 
        nbClassifier.Likelihoods.[label]
            
    let tokenScorer = 
            getTokenScore tokenLikelihoods

    bow 
    |> Seq.choose tokenScorer
    |> Seq.fold (+) (log prior)

let getDocScores 
    (nbClassifier : NbClassifier )
    (bow : BagOfWords) 
    = 

    nbClassifier.Priors
    |> Map.toList
    |> List.map (fun (label, _) -> 
        let docScore = computeDocScore label nbClassifier bow
        label, docScore)

(**
### Train test split
*)

type TokenizerParams = 
    { ReturnThreshold : float
      NGrams : int
      TopKPctTfIdf : float
      TrainSplit : float }

let trainTestSplit 
    (tokenizerParams : TokenizerParams) 
    (ears : EarningsAnnouncementReturn []) 
    : (Label * BagOfWords) [] * (Label * BagOfWords) [] =

    let generateData, split = 
        // Label call and generate Bag-of-Words
        getFeatureAndLabel tokenizerParams.NGrams tokenizerParams.ReturnThreshold,
        // Split dataset (Train)
        getTrainTest tokenizerParams.TrainSplit
    
    ears
    |> Seq.choose generateData
    |> split
    // Tf-Idf filter
    |> applyTfIdfFilter tokenizerParams.TopKPctTfIdf

(**
### Classifier
*)

let makeClassifier 
    (trainSample : (Label *BagOfWords) []) 
    : NbClassifier = 
    
    let vocab, labelPriors = 
        getCorpusVocab trainSample,
        getPriors trainSample

    let labelLikelihoods = 
        Map (getLabelBows vocab trainSample)
        |> getLabelLikelihoods
 
    { Likelihoods = Map labelLikelihoods 
      Priors = Map labelPriors }

let classifyWith 
    (nbClassifier : NbClassifier)
    (bow : BagOfWords) : Label = 
              
    getDocScores nbClassifier bow
    |> Seq.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate 
    (nbClassifier : NbClassifier)
    (testSamples : (Label * BagOfWords) [])
    : Accuracy =
    
    let classifier = classifyWith nbClassifier

    testSamples
    |> PSeq.averageBy (fun (label, bow) ->  
        if classifier bow = label then 1. 
        else 0.)

(**
### Model 1
*)

// Model 1
let tokenizer11 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.15
      TrainSplit = 0.9 }

let train11, test11 = trainTestSplit tokenizer11 myEars
let nbClassifier11 = makeClassifier train11
let trainEval11 = evaluate nbClassifier11 train11
let testEval11 = evaluate nbClassifier11 test11