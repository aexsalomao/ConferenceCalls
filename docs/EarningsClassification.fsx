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

#load "Types.fsx"

open Types
open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

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

let nGramsTokenizer (nGram : int)
                    (paragraph : string) = 
    paragraph
    // Detect sentence boundaries
    |> splitParagraph
    |> Array.collect (fun phrase ->
        phrase
        // Normalize
        |> getOnlyWords
        |> expandContractions
        // Tokenize
        |> nGrams nGram
        // Stop words removal
        |> Array.choose removeStopWords
        // Empty string removal
        |> Array.filter (fun xs -> xs.Length <> 0))

let generateBow (nGram : int)
                (call : EarningsCall) = 
    call.Transcript
    |> Seq.collect (nGramsTokenizer nGram)
    // Bag-of-words representation
    |> Seq.countBy id
    |> Seq.toArray

myEars
|> Seq.tryFind (fun xs -> xs.EarningsCall.CallId.Ticker = "TSLA")
|> Option.map (fun xs -> 
    generateBow 2 xs.EarningsCall 
    |> Array.sortByDescending snd
    |> Array.take 10)

(**
### Splitting Data: Train and Test sets
*)

let makeLabel earVal thresh = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let getTrainTest cutoffPct ears = 
    let rnd = System.Random(0)
    ears
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.toArray
    |> fun xs -> 
        let cutoff = float xs.Length * cutoffPct
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]

let generateFeatureAndLabel (nGram : int)
                            (thresh : float) 
                            (ear : EarningsAnnouncementReturn): (Label * BagOfWords) option = 
    match ear.Ear with
    // Filter for significant ears
    | Some earVal when abs earVal >= thresh -> 
        Some (makeLabel earVal thresh, 
              generateBow nGram ear.EarningsCall)
    | _ -> None

(**
### Term Frequency - Inverse Document Frequency (Bag-of-words approach)
*)

(**
#### Term frequency (Tf)
*)

let termFreq (bow : BagOfWords) : (Token * float) [] = 
    let docTokenCounts = Seq.sumBy snd bow

    bow
    |> Seq.map (fun (token, count) -> 
        let tf = (float count)/(float docTokenCounts)
        token, tf)
    |> Seq.toArray

(**
#### Inverse document frequency (Idf)
*)

let inverseDocFreq (trainSamples : (Label * BagOfWords) []) : Map<Token, float> = 
    let n = trainSamples.Length

    trainSamples
    |> Array.collect (fun (_, bow) -> 
        bow
        // Convert token counts to 1
        |> Array.map (fun (token, _) -> token, 1))
    // Group by token
    |> Array.groupBy fst
    // Sum occurences and compute idf
    |> Array.map (fun (token, tokenCounts) -> 
        
        let idf = 
            tokenCounts
            |> Array.sumBy snd
            |> fun numDocsWithToken -> 
                (float n) / (float numDocsWithToken) 
                |> log

        token, idf)
    |> Map

(**
#### Term frequency - inverse document frequency (TfIdf)
*)

let tfIdf (idf : Map<Token, float>) 
          (idfPrior : float)
          (bow : BagOfWords) = 

    termFreq bow
    |> Seq.choose (fun (token, tf) -> 
        match idf.TryFind token with
        // Word appeared in train
        | Some idf -> 
            let tfIdf = tf * idf
            Some (token, tfIdf)
        // Word did not appear in train (very rare word -> 0. ?)
        | None -> 
            let tfIdf = tf * idfPrior
            Some (token, tfIdf))
    |> Seq.toArray

(**
#### Filter Train and Test by TfIdf
*)

let filterByTfIdf (idf : Map<Token, float>)
                  (idfPrior : float)
                  (thresh : float)
                  (sample : Label * BagOfWords) = 

    let label, bow = sample

    let filteredBow = 
        tfIdf idf idfPrior bow
        |> Seq.sortByDescending snd
        |> Seq.map fst
        |> Seq.take ((thresh * float bow.Length) |> int)
        |> Set
        |> fun highValueTokens -> 
            bow
            |> Array.filter (fun (token, _) -> 
                highValueTokens.Contains token)
                
    (label, filteredBow)

(**
### Multinomial Naive Bayes: Bag of words approach
*)

(**
### Priors
*)

let getPriors (trainSample : (Label * BagOfWords)[]): Map<Label, Prior>= 
    let n = trainSample.Length
    trainSample
    |> Array.groupBy fst
    |> Array.map (fun (label, labelsFreqs) -> 
        let prior =  (float labelsFreqs.Length)/(float n)
        label, prior)
    |> Map

(**
#### Vocabulary
*)

// Vocabulary from filtered corpus
let getCorpusVocab (trainSamples : (Label * BagOfWords) [])= 
    trainSamples
    |> Array.collect (fun (_, bow) -> 
        bow
        |> Array.map fst)
    |> Set

(**
#### Combining samples to form a bag-of-words for each label
*)


let labels = 
    [|
        Positive
        Negative
    |]

// Creates Label BoW
let combineBow (samples : (Label * BagOfWords) []) = 
    samples 
    |> Seq.collect snd
    |> Seq.groupBy fst
    |> Seq.map (fun (token, tokenCounts) -> 
        let totalCount = Seq.sumBy snd tokenCounts
        token, totalCount)
    |> Map

// Laplace correction
let fillMissingVocab (vocab : Set<Token>)
                     (labelBow : Map<Token , Count>): BagOfWords = 
    vocab
    |> Set.toArray
    |> Array.choose (fun vocabToken ->
        match labelBow.TryFind vocabToken with
        | Some count -> Some (vocabToken, count + 1)
        | None -> Some (vocabToken, 1))

let getLabelBows (vocab : Set<Token>)
                 (samples : (Label * BagOfWords) []): Map<Label, BagOfWords> = 
    samples
    |> Array.groupBy fst
    |> Array.map (fun (label, samples) -> 
        let bow = 
            fillMissingVocab vocab (combineBow samples)
        label, bow)
    |> Map

(**
### Token Likelihoods by Label
*)

let getLabelLikelihoods (labelBows : Map<Label, BagOfWords>): Map<Label, Map<Token, Likelihood>> = 
    labelBows
    |> Map.toArray
    |> Array.map (fun (label, bow) ->
        let n = Seq.sumBy snd bow
        let likelihoods = 
            bow
            |> Array.map (fun (token, count) -> 
                token, (float count)/(float n))
        label, likelihoods |> Map)
    |> Map

(**
### Token score
*)

(**
Take logs to prevent underflow
*)

let getTokenScores (tokenLikelihoods : Map<Token, Likelihood>)
                   (uniformPrior : float)
                   (tokenCount : Token * Count) = 
    
    let token, count = tokenCount

    match tokenLikelihoods.TryFind token with
    | Some l -> 
        let tokenScore = log (l ** (float count))
        Some tokenScore
    | None -> 
        let tokenScore = log (uniformPrior ** (float count))
        Some tokenScore

let getDocScores (labelLikelihoods : Map<Label, Map<Token, Likelihood>>)
                 (priors : Map<Label, float>)
                 (bow : BagOfWords) = 

    labels
    |> Array.choose (fun label -> 
        match labelLikelihoods.TryFind label with
        | Some tL -> 
            let avgLikelihood = 
                tL
                |> Map.toArray
                |> Array.averageBy snd

            bow 
            |> Array.choose (getTokenScores tL avgLikelihood)
            |> Array.fold (+) (log priors.[label])
            |> fun docScore ->
                Some (label, docScore)
        | None -> None)

(**
### Classify
*)

let classifyWith (labelLikelihoods : Map<Label, Map<Token, Likelihood>>)
                 (priors : Map<Label, float>)
                 (bow : BagOfWords): Label = 
              
    getDocScores labelLikelihoods priors bow
    |> Seq.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (classifier: BagOfWords -> Label)
             (testSamples: (Label * BagOfWords) []): float =
    testSamples
    |> Seq.averageBy (fun (label, bow) ->  
        if classifier bow = label then 1. 
        else 0.)

(**
### Build Classifier
*)

let buildClassifier (trainSample : (Label *BagOfWords) []) = 
    
    let vocab, priors = 
        getCorpusVocab trainSample,
        getPriors trainSample

    let labelLikelihoods = 
        getLabelBows vocab trainSample
        |> getLabelLikelihoods
 
    classifyWith labelLikelihoods priors

(**
### Split Data: Train and Test
*)

type PreprocessingParams = 
    { 
        ReturnThreshold : float
        NGrams : int
        TopKPctTfIdf : float
        TrainSplit : float
    }

let applyTfIdfFilter (topKPctIfIdf : float)
                     (train, test) =

    let corpusIdf = inverseDocFreq train
    let idfPrior = 
        corpusIdf
        |> Map.toArray
        |> Array.averageBy snd

    let tfIdfFilter = 
        filterByTfIdf corpusIdf idfPrior topKPctIfIdf

    Array.map tfIdfFilter train,
    Array.map tfIdfFilter test

let trainTestSplit (modelParams : PreprocessingParams)
                   (ears : EarningsAnnouncementReturn []) =

    let generateData, splitData = 
        // Label call and generate Bag-of-Words
        generateFeatureAndLabel modelParams.NGrams modelParams.ReturnThreshold,
        // Split dataset (Train)
        getTrainTest modelParams.TrainSplit
    
    ears
    |> Seq.choose generateData
    |> splitData
    // Tf-Idf filter
    |> applyTfIdfFilter modelParams.TopKPctTfIdf

let evaluateModel (model : PreprocessingParams) 
                  (ears : EarningsAnnouncementReturn []) = 
    // Split data
    let train, test = trainTestSplit model ears

    // Build classifier
    let classifier = buildClassifier train

    // Evaluate
    evaluate classifier train,
    evaluate classifier test

(**
#### Model 1
*)

// Set model parameters
let model1 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.75
      TrainSplit = 0.8 }

let m1 = evaluateModel model1 myEars

(**
#### Model 2
*)

// Set model parameters
let model2 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.5
      TrainSplit = 0.8 }

// Evaluate
let m2 = evaluateModel model2 myEars

(**
#### Model 3
*)

// Set model parameters
let model3 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.25
      TrainSplit = 0.8 }

// Evaluate
let m3 = evaluateModel model3 myEars

(**
#### Model 4
*)

let model4 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.25
      TrainSplit = 0.8 }

// Evaluate
let m4 = evaluateModel model4 myEars

(**
#### Model 5
*)

let model5 = 
    { ReturnThreshold = 0.05
      NGrams = 3
      TopKPctTfIdf = 0.2
      TrainSplit = 0.8 }

// Evaluate
let m5 = evaluateModel model5 myEars