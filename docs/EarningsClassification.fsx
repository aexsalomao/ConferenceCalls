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
#load "TextPreprocessing.fsx"

open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

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

let earsHist (ears : float []) 
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

let makeBow (nGram : int)
                (call : EarningsCall) = 
    call.Transcript
    |> Seq.collect (nGramsTokenizer nGram)
    // Bag-of-words representation
    |> Seq.countBy id
    |> Seq.toArray

myEars
|> Seq.tryFind (fun xs -> xs.EarningsCall.CallId.Ticker = "TSLA")
|> Option.map (fun xs -> 
    makeBow 2 xs.EarningsCall 
    |> Array.sortByDescending snd
    |> Array.take 10)

(**
### Splitting Data: Train and Test sets
*)

let makeLabel earVal thresh = 
    if earVal >= thresh then Positive
    elif earVal <= -thresh then Negative
    else Neutral

let getFeatureAndLabel (nGram : int)
                       (thresh : float) 
                       (ear : EarningsAnnouncementReturn) : (Label * BagOfWords) option = 
    match ear.Ear with
    // Filter for significant ears
    | Some earVal when abs earVal >= thresh -> 
        Some (makeLabel earVal thresh, 
              makeBow nGram ear.EarningsCall)
    | _ -> None

let getTrainTest splitCuttof ears = 
    let rnd = System.Random(0)
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

let termFreq (bow : BagOfWords) : (Token * float) [] = 
    let docTokenCounts = Seq.sumBy snd bow

    bow
    |> Array.map (fun (token, count) -> 
        let tf = (float count)/(float docTokenCounts)
        token, tf)

(**
#### Inverse document frequency (Idf)
*)

let inverseDocFreq (trainSamples : (Label * BagOfWords) []) : Map<Token, float> = 
    let n = trainSamples.Length

    trainSamples
    |> Array.collect (fun (_, bow) -> 
        bow
        |> Array.map fst)
    |> Array.countBy id
    |> Array.map (fun (token, numDocsWithToken) -> 
        let idf = log ((float n) / (float numDocsWithToken))
        token, idf)
    |> Map

(**
#### Term frequency - inverse document frequency (TfIdf)
*)

let tfIdf (idf : Map<Token, float>) 
          (idfPrior : float)
          (bow : BagOfWords) = 

    termFreq bow
    |> Array.choose (fun (token, tf) -> 
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
        let tokenLikelihoods = 
            bow
            |> Array.map (fun (token, count) -> 
                token, (float count)/(float n))
        label, tokenLikelihoods |> Map)
    |> Map

(**
### Token score
*)

let getTokenScore (tokenLikelihoods : Map<Token, Likelihood>)
                  (uniformPrior : float)
                  (tokenCount : Token * Count) : TokenScore = 
    
    let token, count = tokenCount

    match tokenLikelihoods.TryFind token with
    | Some l -> 
        l ** (float count) 
        |> log
       
    | None -> 
        uniformPrior ** (float count) 
        |> log

let computeDocScore (tokenLikelihoods : Map<Token, Likelihood>) 
                    (priors : Map<Label, float>)
                    (label : Label)
                    (bow : BagOfWords) : DocScore = 
                
    let tokenScorer = 
        tokenLikelihoods
        |> Map.toArray
        |> Array.averageBy snd
        |> fun avgLikelihood -> 
            getTokenScore tokenLikelihoods avgLikelihood

    bow 
    |> Array.map tokenScorer
    |> Array.fold (+) (log priors.[label])

let getDocScores (labelLikelihoods : Map<Label, Map<Token, Likelihood>>)
                 (priors : Map<Label, float>)
                 (bow : BagOfWords) : (Sentiment * DocScore) [] = 
    priors
    |> Map.toArray
    |> Array.map fst
    |> Array.choose (fun label -> 
        match labelLikelihoods.TryFind label with
        | Some tokenLikelihoods -> 
            let docScore = computeDocScore tokenLikelihoods priors label bow
            Some (label, docScore)
        | None -> None)

(**
### Train test split
*)

type TokenizerParams = 
    { 
        ReturnThreshold : float
        NGrams : int
        TopKPctTfIdf : float
        TrainSplit : float
    }

let trainTestSplit (tokenizerParams : TokenizerParams) 
                   (ears : EarningsAnnouncementReturn []) : (Label * BagOfWords) [] * (Label * BagOfWords) [] =

    let generateData, splitData = 
        // Label call and generate Bag-of-Words
        getFeatureAndLabel tokenizerParams.NGrams tokenizerParams.ReturnThreshold,
        // Split dataset (Train)
        getTrainTest tokenizerParams.TrainSplit
    
    ears
    |> Seq.choose generateData
    |> splitData
    // Tf-Idf filter
    |> applyTfIdfFilter tokenizerParams.TopKPctTfIdf

(**
### Classifier
*)

type NbClassifier =
    {
        LabelLikelihoods :  Map<Label, TokenLikelihoods>
        Priors : Map<Label, float>
    }

let makeClassifier (trainSample : (Label *BagOfWords) []) : NbClassifier = 
    
    let vocab, priors = 
        getCorpusVocab trainSample,
        getPriors trainSample

    let labelLikelihoods = 
        getLabelBows vocab trainSample
        |> getLabelLikelihoods
 
    { LabelLikelihoods = labelLikelihoods 
      Priors = priors }

let classifyWith (nbClassifier : NbClassifier)
                 (bow : BagOfWords) : Label = 
              
    getDocScores nbClassifier.LabelLikelihoods nbClassifier.Priors bow
    |> Seq.maxBy snd
    |> fst

(**
### Evaluate
*)

let evaluate (nbClassifier : NbClassifier)
             (testSamples : (Label * BagOfWords) []): float =
    
    let classifier = classifyWith nbClassifier

    testSamples
    |> Seq.averageBy (fun (label, bow) ->  
        if classifier bow = label then 1. 
        else 0.)

(**
### Model 1
*)

let tokenizer1 = 
    { ReturnThreshold = 0.05
      NGrams = 1
      TopKPctTfIdf = 0.10
      TrainSplit = 0.9 }

let train1, test1 = trainTestSplit tokenizer1 myEars
let nbClassifier1 = makeClassifier train1
evaluate nbClassifier1 train1
evaluate nbClassifier1 test1