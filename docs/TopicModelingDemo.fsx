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
#r "nuget: MathNet.Numerics.FSharp, 4.15.0"

open System

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
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

let train, test = 
    let rnd = System.Random(42)
    readJson ("data-cache/LabeledTranscriptsFullSample.json")
    |> Seq.sortBy (fun _ -> rnd.Next())
    |> Seq.toArray
    |> fun xs -> 
        let cutoff = float xs.Length * 0.8
        xs.[.. int cutoff], xs.[int cutoff + 1 ..]


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
#### 2) Lemmatiazation/Stemming
*)

(**
- Analyze words as a single root, e.g, "dissapointment" to "dissapoint"
- Porters algorithm
*)

(**
#### 3) Tokenization
*)

(**
- Split each article into a list of words or phrases
- NGrams
*)

(**
#### 4) Bag of words
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
    {Ticker: string; Exchange: string} 

type WordCount = 
    {Word: string; Count: int}

type Sentiment =
    | Positive
    | Negative

type Call = 
    { CallId: CallId
      WordCount: WordCount []
      Signal: float } with
    
    member this.Flag =
        if this.Signal > 0. then Positive
        else Negative

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
        |> Seq.countBy id
        |> Seq.map (fun (word, count) -> {Word=word; Count=count})
        |> Seq.toArray

    { CallId = callId
      WordCount = wordCount
      Signal = signal }

let trainCalls, testCalls = 
    train
    |> Array.Parallel.map generateCall,

    test 
    |> Array.Parallel.map generateCall

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

Before computing any scores, we need to first find out the frequency and "occurence" of each word or text item in the corpus of documents.

While the frequency of each text item or word is simply its total count across all documents, an item's occurence is equivalent to the total count of *documents* that include text item *j*.

We can then define two variants of screening scores:

1. Screening score based on total word frequency:

$$f_{j} = \frac{{\text{count of word } j \text{ in articles with } sgn(y) = +1 }}{\text{count of word } j \text{ in all articles}} $$

2. Screening score based on word occurence across documents:

$$f_{j}^{*} = \frac{{\text{count of articles including word } j \text{ in articles with } sgn(y) = +1 }}{\text{count of articles including word } j }$$

*)

type CountType = 
    | Frequency
    | Occurence

type TextItemScreening =
    { TextItem: string
      Score: float
      Count: float 
      CountType : CountType}

/// Vector of item counts per Group (Flag) (Bag of words per group)
let itemOccurenceByGroup, itemFrequencyByGroup = 
    trainCalls
    |> Seq.groupBy (fun xs -> xs.Flag)
    |> Seq.map (fun (group, callsOfGroup) -> 
        callsOfGroup 
        |> Seq.collect (fun xs -> xs.WordCount)
        |> Seq.groupBy (fun xs -> xs.Word)
        |> Seq.map (fun (wordId, wordCounts) ->
            wordCounts
            |> fun xs -> 
                // Occurence (# of articles that word j appears)
                (wordId, xs |> Seq.length),
                // Frequency (total count of word j in all articles)
                (wordId, xs |> Seq.sumBy (fun xs -> xs.Count)))
        |> Seq.toArray
        |> fun xs ->
            (group, xs |> Array.map fst |> Map), 
            (group, xs |> Array.map snd |> Map))
    |> Seq.toArray
    |> fun xs -> 
        (xs |> Array.map fst |> Map),
        (xs |> Array.map snd |> Map)

/// Frequency/Occurence finder
let countOfItemInGroup (group: Sentiment)
                       (wordSentimentMap : Map<Sentiment, Map<string, int>>)
                       (item: string) = 
    wordSentimentMap.TryFind group
    |> Option.bind (fun xs -> xs.TryFind item)

/// Vocabulary (training set only)
let vocabulary =
    trainCalls
    |> Seq.collect (fun xs -> xs.WordCount |> Array.map (fun xs -> xs.Word))
    |> Seq.distinct
    |> Seq.toArray

/// Get scores from given word sentiment map (Frequency or Occurence)
let getScores (wordSentimentMap: Map<Sentiment, Map<string, int>>)
              (countType: CountType) = 

    let getItemScore item = 
        let generateItemScore item score count = 
            {TextItem = item; Score = score; Count = count; CountType = countType }

        let posN, negN = 
            countOfItemInGroup Positive wordSentimentMap item,
            countOfItemInGroup Negative wordSentimentMap item 

        match posN, negN with
        | Some p, Some n -> 
            let score, count = (float p) / ((float p) + (float n)), float (p + n)
            Some (generateItemScore item score count)
        | Some p, None -> 
            let score, count = 1., float p
            Some (generateItemScore item score count)
        | None, Some n -> 
            let score, count = 0., float n
            Some (generateItemScore item score count)
        | _ ->  None
    
    vocabulary
    // Compute text item scores
    |> Array.Parallel.choose getItemScore
    |> Array.map (fun xs -> xs.TextItem, xs)
    |> Map

let itemOccurenceScores, itemFrequencyScores = 
    getScores itemFrequencyByGroup Frequency, 
    getScores itemOccurenceByGroup Occurence

(**
Histogram: Item scores
*)

itemFrequencyScores
|> Map.toArray
|> Array.map (fun (word, xs) -> xs.Score)
|> Array.filter (fun xs -> xs > 0.25 && xs < 0.75)
|> Chart.Histogram
|> Chart.Show

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
let getChargedItems alphaLower alphaUpper kappaPct = 

    // Upper and lower score thresholds
    let upperThresh, lowerThresh = 
        trainCalls
        |> Array.filter (fun xs -> xs.Flag = Positive)
        |> fun xs -> float xs.Length / float train.Length
        |> fun pieHat -> (pieHat + alphaUpper), (pieHat - alphaLower)

    // Count of text item in all articles
    let kappa = kappaPct * float train.Length

    // Screening
    let isCharged itemInfo =
        (itemInfo.Score >= upperThresh || itemInfo.Score <= lowerThresh) && (itemInfo.Count >= kappa)

    itemScores
    |> Map.toSeq
    |> Seq.filter (fun (_, itemInfo) -> isCharged itemInfo)
    |> Seq.toArray

    /// Making sure number of words in each group is equal
    |> fun allWords ->
        let negativeWords = 
            allWords 
            |> Array.filter (fun (_, score) -> score.Score <= lowerThresh)
            |> Array.sortBy (fun (_, score) -> score.Score)
        
        let positiveWords = 
            allWords 
            |> Array.filter (fun (_, score) -> score.Score >= upperThresh)
            |> Array.sortByDescending (fun (_, score) -> score.Score)

        if negativeWords.Length > positiveWords.Length 
        then
            negativeWords
            |> Array.take (positiveWords.Length)
            |> fun xs -> 
               Array.concat [|xs; positiveWords|]
        else 
            positiveWords
            |> Array.take (negativeWords.Length)
            |> fun xs -> 
                Array.concat [|xs; negativeWords|]

let alphaLower, alphaUpper, kappa  = (0.001, 0.001, 0.75)
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
        |> Array.Parallel.map (fun (chargedWord, _) -> 
            match callMap.TryFind chargedWord with
            | Some wordCount -> wordCount
            | None -> {Word=chargedWord;Count = 0})
        |> Array.sortBy (fun xs -> xs.Word)
    
    { CallId = call.CallId
      WordCount = filteredItemCounts
      Signal = call.Signal }

let chargedTrain = 
    trainCalls
    |> Array.Parallel.map filterCall

let getDocumentTermMatrix (calls: Call []) = 
    calls
    |> Array.map (fun xs -> 
        xs.WordCount 
        |> Array.map (fun xs -> float xs.Count))
    |> matrix

let chargedDocumentTermMatrix = getDocumentTermMatrix chargedTrain

chargedDocumentTermMatrix.RowCount
chargedDocumentTermMatrix.ColumnCount

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

$$\widehat{s}_{i} = \sum_{j \in \widehat{S}}{d_{j, i}}$$

Estimate W using the standardized ranks of returns. For each each article $$i$$ in the training sample $$i = 1, ..., n$$:

$$\widehat{p}_{i} = \frac{\text{rank of } y_{i} \text{ in } \{y_{l}\}_{l=1}^{n}}{n}$$

*)

(**
## Estimator $\widehat{O}$
*)

(**
#### $H$
*)

(**
$$\widehat{H} = [\widehat{h_{1}}, \widehat{h_{2}},..., \widehat{h_{3}}]$$

$$\widehat{h_{i}} = \frac{d_{[\widehat{S}], i}}{\widehat{s}_{i}} \text{      } \widehat{s}_{i} = \sum_{j \in \widehat{S}}{d_{j, i}}$$
*)

let bigH = 
    chargedTrain
    |> getDocumentTermMatrix
    |> fun m ->
        m.ToColumnArrays()
        |> Array.map (fun itemCounts -> 
            let sumOfItemCounts = 
                Array.sum itemCounts

            itemCounts 
            |> Array.map (fun xs -> 
                xs / sumOfItemCounts))
        |> matrix

bigH.RowCount
bigH.ColumnCount

(**
#### $W$
*)

(**
$$\widehat{W} = \begin{bmatrix} \widehat{p_{1}} & \widehat{p_{2}} & \cdots & \widehat{p_{n}} \\ 1 - \widehat{p_{1}} & 1 - \widehat{p_{2}} & \cdots & 1 -\widehat{p_{n}} \end{bmatrix}$$

$$\widehat{p}_{i} = \frac{\text{rank of } y_{i} \text{ in } \{y_{l}\}_{l=1}^{n}}{n}$$
*)

let bigW = 

    let n = chargedTrain.Length

    chargedTrain
    |> Array.sortBy (fun xs -> xs.Signal)
    |> Array.mapi (fun i _ -> 
        (float (i + 1)/float n))
    |> fun xs -> 
        matrix [|xs; xs |> Array.map (fun p -> (1. - p))|]

bigW.RowCount
bigW.ColumnCount

(**
#### $O$
*)          

(**
$$\widehat{O} = [\widehat{h_{1}}, \widehat{h_{2}},\ldots, \widehat{h_{n}}] \widehat{W}^{'} (\widehat{W}\widehat{W}^{'})^{-1}$$
*)

let bigO = 
    
    let h, w, w' = bigH, bigW, bigW.Transpose()
    let ww' = w.Multiply(w')

    h.Multiply(w').Multiply(ww'.Inverse())
    |> Matrix.toColArrays
    |> Array.map (fun col -> 
        col
        |> Array.map (fun xs -> if xs < 0. then 0. else xs)
        |> fun onlyPositiveVals -> 
            let norm = Array.sum onlyPositiveVals
            onlyPositiveVals 
            |> Array.map (fun xs -> xs / norm))
    |> matrix
    |> fun m -> m.Transpose()

bigO.RowCount
bigO.ColumnCount

(**
## 3. Scoring New Articles
*)

(**
Estimating $p$ (sentiment score) for new articles using maximum likelihood estimation:
$$\widehat{p} = \arg\max_{p\in[\,0, 1]\,} \left\{\hat{s}^{-1} \sum_{j \in \widehat{S}}{d_{j}\log \left(p \widehat{O}_{+, j} + (\,1-p)\,\widehat{O}_{-, j}\right) + \lambda \log \left(p\left(1 - p \right)\right) \right\}$$
*)

(**
Optimization
*)

let bigOArr = 
    bigO.ToRowArrays()

let objF (call: Call) (p: float) (lambda: float) = 

    let filteredCall = filterCall call
    
    let sHat = 
        filteredCall
        |> fun xs -> 
            xs.WordCount 
            |> Array.sumBy (fun xs -> xs.Count) 
            |> fun xs -> (1. / float xs)

    filteredCall.WordCount
    |> Array.mapi (fun i xs -> 
        let pos = p * bigOArr.[i].[0]
        let neg = (1. - p) * bigOArr.[i].[1]
        let d = float xs.Count
        d * log (pos + neg))
    |> fun expr ->
        (sHat *(Array.sum expr)) + (lambda * log (p * (1. - p)))
        
let computeScore (call: Call) = 
    [|0. .. 0.01 .. 1.|]
    |> Array.map (fun scoreP -> (scoreP, call.Signal), (objF call scoreP 0.005))
    |> Array.maxBy snd

let classTest = 
    test
    |> Array.sortBy (fun xs -> xs.Signal)
    |> Array.take 100
    |> Array.map (fun xs -> 
        let res = computeScore xs
        res)

(**
DiffSharp demo
*)

#r "nuget: DiffSharp-lite, 1.0.0-preview-987646120"

open DiffSharp
open DiffSharp.Optim

let computeScore' (call: Call) (x: Tensor) =

    let filteredCall = filterCall call

    let sHat = 
        filteredCall
        |> fun xs -> 
            xs.WordCount 
            |> Array.sumBy (fun xs -> xs.Count) 
            |> fun xs -> (1. / float xs)

    let scoreP = x.[0]

    filteredCall.WordCount
    |> Array.mapi (fun i xs -> 
        let pos = scoreP * bigOArr.[i].[0]
        let neg = (1. - scoreP) * bigOArr.[i].[1]
        let d = float xs.Count
        d * log (pos + neg))
    |> Array.sum
    |> fun sumExpr -> 
        (sHat * sumExpr) + (0.001 * log (scoreP * (1. - scoreP)))
        |> fun xs -> xs * -1.
    

let lr, momentum, iters, threshold = 1e-3, 0.5, 1000, 1e-3

let scoreFun = computeScore' negativeArticleTrain

let scorePGuess = dsharp.tensor([0.5])

let scoreFx, params' = optim.sgd(scoreFun, scorePGuess, lr=dsharp.tensor(lr), momentum=dsharp.tensor(momentum), nesterov=true, iters=iters, threshold=threshold)

