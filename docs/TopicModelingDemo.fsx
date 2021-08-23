(**
---
title: Predicting Returns with text data
category: Scripts
categoryindex: 2
index: 4
---
*)

(**
## Import packages and load scripts
*)

#r "nuget: Plotly.NET, 2.0.0-preview.6"
#r "nuget: FSharp.Collections.ParallelSeq"
#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"
#r "nuget: MathNet.Numerics, 4.15.0"
#r "nuget: MathNet.Numerics.FSharp, 4.15.0"

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

(**
## Read transcripts from json file
*)

type Label = 
    | Positive
    | Negative
    | Neutral

type LabeledTranscript = 
    { TickerExchange: (string * string) 
      EarningsCall: string
      CumulativeReturn: float 
      Label: Label }

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<array<LabeledTranscript>>(json)

let fullSample = 
    readJson ("data-cache/LabeledTranscriptsFullSample.json")

let positiveOrNegativeRand =
    let rnd = System.Random()
    fullSample
    |> Seq.filter (fun xs -> xs.Label <> Neutral)
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.take 1000
    |> Seq.toArray

(**
## Text Preprocessing: Bag of words
*)

(**
See `TextPreprocessing.fsx`
*)

(**
#### 1) Normalization
*)

(**
- Change all words in each article to lower case letters
- Expand contractions such as "haven't" to "have not"
- Delete numbers, punctuation, special symbols, and non-English words
*)

(**
#### 2) Lemmatiazation/Stemming (Porters Algorithm)
*)

(**
- Analyze words as a single root, e.g, "dissapointment" to "dissapoint"
*)

(**
#### 3) Tokenization
*)

(**
- Split each article into a list of words or phrases
*)

(**
#### 4) "Bag of words"
*)

(**
- Transform each block of text to a vector of word counts
*)

(**
#### Train and test sets
*)

#load "TextPreprocessing.fsx"
open Preprocessing.Normalization
open Preprocessing.Tokenization

type CallId = 
    { Ticker: string
      Exchange: string } 

type Sentiment =
    | Positive
    | Negative

type WordCount = 
    {Word: string; Count: int}

type Call = 
    { CallId: CallId
      WordCount: WordCount []
      Signal: float } with
    
    member this.Flag =
        if this.Signal > 0. then Positive
        else Negative

let train, test = 
    let generateCall (xs: LabeledTranscript) = 
        let callId = {Ticker = fst xs.TickerExchange ; Exchange = snd xs.TickerExchange}
        let signal = xs.CumulativeReturn
        let wordCount = 
            xs.EarningsCall
            // Normalization
            |> getOnlyWords
            |> expandContractions
            // Tokenization
            |> nGrams 1
            // Bag of words
            |> Array.countBy id
            |> Array.map (fun (word, count) -> {Word=word; Count=count})

        { CallId = callId
          WordCount = wordCount
          Signal = signal }

    positiveOrNegativeRand
    |> Array.map generateCall
    |> fun calls -> 
        let cutoff = float calls.Length * 0.8
        calls.[.. int cutoff], calls.[int cutoff + 1 ..]

(**
## Introduction to Topic modeling
*)

(**
Topic detection or **topic modeling** is a technique of automatically extracting meaning from texts
by identifying recurrent themes or topics.

Topic modeling is a method for analyzing large volumes of unlabeld text data. It helps in:
- Discovering hidden topical patterns that are present across the collection

- Annotating documents according to these topics

- Using these annotations to organize, search and summarize texts

A *topic* consists of a cluster of words that frequently occur together.
This is essentially a clustering problem - we can think of both words and documents as being clustered.

There are many techniques that are used to obtain topic models. One of the commonly used is 
**Latent Dirichlet Allocation (LDA)**
*)

(**
## Predicting Returns with Text Data
*)

(**
**SESTM: A Supervised Sentiment Extraction Algorithm**

Methodology:

1. Feature selection: create a set of sentiment-charged words via *predictive (correlation) screening*
2. Assign prediction/sentiment weights to these words via a supervised topic model (i.e. estimate positive and negative sentiment topics)
3. Aggregate terms into an article-level predictive score via penalized likelihood.

- Model is motivated by the view that return-predictive content of a given event is 
reflected *both* in the news article text and in the returns of related assets.

- Method has an objective of extracting general *return predictive* content from text.
*)

(**
**Advantages**

- Simplicity: only requires standard econometric techniques such as correlation analysis and maximum likelihood estimation. 
Additionally, unlike other deep learning approaches, the proposed *supervised* learning approach is entirely "white-box".

- Minimal computing power required.

- Free of any pre-existing sentiment dictionary (polarized words, sentiment lexicons, etc...). No use of ad hoc word-weighting schemes.

Bottom line: A sentiment scoring model is constructed from the *joint* behaviour of 
article text and stock returns.
*)

(**
**Theoretical Reusults**

- The guarantee of retrieving a sentiment dictionary from training data via correlation screening.

- The derivation of sharp error bounds for parameter estimation. The error bounds depend on the scale
of the corpus (e.g., size of the vocabulary, total number of text documents, average number of words 
per document, etc.), and the strength of sentiment signals (e.g., the sentivity of returns to sentiment, 
sensitivity of text generation to sentiment, etc.).

- The error of predicting the sentiment score of a newly arriving article is both derived and quantified.
*)

(**
### A Probabilistic Model for Sentiment Analysis
*)

(**
### 1) Screening for Sentiment-Charged words
*)

(**
Objective: Isolate the subset of sentiment-charged words (remove sentiment-neutral words, i.e. noise).

Intuitively, if a word frequently co-occurs in articles that are accompanied
by positive returns, that word is likely to convey positive sentiment.

Methodology:

1. Calculate the frequency with which each word (or phrase) *j* co-occurs with a positive
return. (screening-score $f_{j}$)

2. Compare $f_{j}$ with proper thresholds and create the sentiment-charged set of words $S$.
*)

(**
#### 1A) Screening Score

$$f_{j} = \frac{{\text{count of word } j \text{ in articles with } sgn(y) = +1 }}{\text{count of word } j \text{ in all articles}} $$

- Form of *marginal screening statistics*
*)

type TextItemScreening =
    { TextItem: string
      Score: float
      Count: float }

/// Corpus vocabulary (using only training set)
let vocabulary = 
    train
    |> Array.collect (fun xs -> xs.WordCount |> Array.map (fun xs -> xs.Word))
    |> Array.distinct

/// Vector of item counts per Group (Flag) (Bag of words per group)
let itemCountsByGroup = 
    train
    |> Seq.groupBy (fun xs -> xs.Flag)
    |> Seq.map (fun (group, callsOfGroup) -> 
        
        let wordCounts = 
            callsOfGroup 
            |> Seq.collect (fun xs -> xs.WordCount)
            |> Seq.groupBy (fun xs -> xs.Word)
            |> Seq.map (fun (wordId, wordCounts) -> 
                wordId, wordCounts |> Seq.sumBy (fun xs -> xs.Count))
            |> Seq.toArray

        group, wordCounts |> Map)
    |> Seq.toArray
    |> Map

/// Item Scores Map (used to filter for Sentiment charged words)
let itemScores = 
    let getItemScore item =
       
        let countOfItemInGroup item group = 
            itemCountsByGroup.TryFind group
            |> Option.bind (fun itemsCountMap -> itemsCountMap.TryFind item)
            
        let positiveCount, negativeCount = 
            countOfItemInGroup item Positive, countOfItemInGroup item Negative

        match positiveCount, negativeCount with
        | Some p, Some n -> 
            let score = (float p / (float p + float n))
            let count = p + n
            Some {TextItem = item; Score = score; Count = float count}
        | Some p, None -> 
            Some {TextItem = item; Score = 1.; Count = float p}
        | None, Some n -> 
            Some {TextItem = item; Score = 0.; Count = float n}
        | _ ->  None
    
    vocabulary
    |> Seq.choose getItemScore
    |> Seq.map (fun xs -> xs.TextItem, xs)
    |> Map

(**
#### 1B) Sentiment-charged set of words

$$\widehat{S} = \{j: f_{j} \geq \widehat{\pi} + \alpha_{+}, \text{ or } f_{j} \leq \widehat{\pi} - \alpha_{-} \} \cap \{ j: k_{j} \geq \kappa\}$$

- $f_{j} = \text{Sentiment-screening score of word } j $
- $\widehat{\pi} = \text{Fraction of articles tagged with a positive return}$
- $\alpha_{+} = \text{Upper sentiment-score threshold}$
- $\alpha_{-} = \text{Lower sentiment-score threshold}$
- $k_{j} = \text{count of word } j \text{ in all articles}$

The thresholds ($\alpha{+}, \alpha{-}, \kappa$) are *hyper-parameters* that can be tuned via cross-validation.
*)

/// Sentiment-charged words
let getChargedItems alphaLower alphaUpper kappa = 

    // Upper and lower score thresholds
    let upperThresh, lowerThresh = 
        train 
        |> Array.filter (fun xs -> xs.Flag = Positive)
        |> fun xs -> float xs.Length / float train.Length
        |> fun pieHat -> (pieHat + alphaUpper), (pieHat - alphaLower)

    // Screening
    let isCharged itemInfo =
        (itemInfo.Score >= upperThresh || itemInfo.Score <= lowerThresh) && (itemInfo.Count >= kappa)

    itemScores
    |> Map.toSeq
    |> Seq.filter (fun (_, itemInfo) -> isCharged itemInfo)
    |> Seq.toArray
    |> Seq.sortBy fst
    |> Seq.toArray

let alphaLower, alphaUpper, kappa  = (0.10, 0.10, 300.)
let chargedItems = getChargedItems alphaLower alphaUpper kappa

chargedItems.Length

(**
#### Filtering original item counts
- Filter train and test
*)

let filterCall (call: Call): Call = 

    let callMap = 
        call.WordCount 
        |> Array.map (fun xs -> xs.Word, xs) 
        |> Map

    let filteredItemCounts = 
        chargedItems
        |> Array.map (fun (chargedWord, _) -> 
            match callMap.TryFind chargedWord with
            | Some wordCount -> wordCount
            | None -> {Word = chargedWord; Count = 0})
    
    { CallId = call.CallId
      WordCount = filteredItemCounts
      Signal = call.Signal }

let chargedTrain = 
    train
    |> Array.map filterCall

chargedTrain.Length

(**
## 2. Learning Sentiment Topics
*)

(**
Fitting a two-topic model to the sentiment-charged counts, `chargedItemCountsById`.

Some notation:

$$\text{Consider a collection of } n \text{ articles and a dictionary of } m \text{ words.}$$

$$d_{i} = \text{word or (phrase) counts of the } i^{th} article$$

$$d_{i, j} = \text{ number of times word } j \text{ occurs in article } i$$

$$D = m \times n \text{ document term matrix}; D = [d_{1}, ..., d{n}]$$

Model:

$$d_{[S], i} \sim \text{Multinomial} (s_{i}, p_{i}O_{+} + (1 - p_{i})O_{-})$$

$$p_{i} = \text{ article's sentiment score, } p_{i} \in [0,1]$$

$$s_{i} = \text{ total count of sentiment-charged words in article } i$$

$$O_{+} = \text{ positive sentiment topic}$$

$$O_{-} = \text{ negative sentiment topic}$$

$$\mathbb{E}h_{i} = \mathbb{E}\frac{d_{[S], i}}{s_{i}} = p_{i}O_{+} + (1 -p_{i})O_{-}$$

Estimate $$H$$ by plugging in $$\widehat{S}$$ from screening step:

$$\widehat{h_{i}} = \frac{d_{[\widehat{S}], i}}{\widehat{s}_{i}}$$

$$\widehat{s}_{i} = \sum_{j \in \widehat{S}}^{}{d_{j, i}$$

Estimate W using the standardized ranks of returns. For each each article $$i$$ in the training sample $$i = 1, ..., n$$:

$$\widehat{p}_{i} = \frac{\text{rank of } y_{i} \text{ in } \{y_{l}\}_{l=1}^{n}}{n}$$

*)

(**
## Constructing estimators
*)

(**
#### Estimating $H$
*)

(**
$$\widehat{H} = [\widehat{h_{1}}, \widehat{h_{2}},..., \widehat{h_{3}}]$$

$\widehat{h_{i}} = \frac{d_{[\widehat{S}], i}}{\widehat{s}_{i}}$

$\widehat{s}_{i} = \sum_{j \in \widehat{S}}^{}{d_{j, i}$
*)

let bigH = 
    chargedTrain
    |> Array.map (fun xs -> 
        let sumOfChargedWords = 
            xs.WordCount
            |> Array.sumBy (fun xs -> xs.Count)
    
        xs.WordCount
        |> Array.map (fun xs -> float xs.Count/ float sumOfChargedWords))
        |> matrix
    
bigH.RowCount
bigH.ColumnCount

(**
#### Estimating $W$
*)

(**
$$\widehat{W} = \begin{bmatrix} \widehat{p_{1}} & \widehat{p_{2}} & \cdots & \widehat{p_{n}} \\ 1 - \widehat{p_{1}} & 1 - \widehat{p_{2}} & \cdots & 1 -\widehat{p_{n}} \end{bmatrix}$$

$$\widehat{p}_{i} = \frac{\text{rank of } y_{i} \text{ in } \{y_{l}\}_{l=1}^{n}}{n}$$
*)

let bigW = 
    let n = train.Length

    train
    |> Array.sortBy (fun xs -> xs.Signal)
    |> Array.mapi (fun i _ -> 
        (float (i + 1)/float n))
    |> fun xs -> 
    
        matrix [|xs; xs |> Array.map (fun p -> (1. - p))|]

(**
#### Estimating $O$
*)

(**
Topic vectors $\widehat{O_{-}}, \widehat{O_{+}}$ 

$$\widehat{O} = [\widehat{h_{1}}, \widehat{h_{2}},\ldots, \widehat{h_{n}}] \widehat{W}^{'} (\widehat{W}\widehat{W}^{'})^{-1}$$

or 

$$\widehat{O} = [\widehat{h_{1}}, \widehat{h_{2}}, \ldots , \widehat{h_{n}}] A $$

$A = \widehat{W}^{'} (\widehat{W}\widehat{W}^{'})^{-1}$
*)

let bigW' = bigW.Transpose()

let bigA = bigW'.Multiply(bigW.Multiply(bigW').Inverse())

let bigO = 
    bigH.Transpose().Multiply(bigA) 
    |> Matrix.toColArrays
    |> Array.map (fun valArr -> 
        valArr
        |> Array.map (fun xs -> if xs < 0. then 0. else xs)
        |> fun onlyPositiveVals -> 
            let norm = Array.sum onlyPositiveVals
            onlyPositiveVals 
            |> Array.map (fun xs -> xs / norm))
    |> matrix
    |> fun xs -> xs.Transpose()

bigO.RowCount

(**
## 3. Scoring New Articles
*)

(**
Estimating $p$ (sentiment score) for new articles using maximum likelihood estimation:


*)