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
#r "nuget: FSharp.Stats"
#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
open FSharp.Stats

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
    |> Seq.take 500
    |> Seq.toArray

let train, test = 
    let cutoff = int (float positiveOrNegativeRand.Length * 0.8) 
    positiveOrNegativeRand.[.. cutoff], positiveOrNegativeRand.[cutoff + 1 ..]

(**
### N-Grams (DocTransformer = string -> string [])
*)

let nGrams (n: int) (text: string) = 
  
    let generateNGram words = 
        let onlyWords = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")
        
        let isWord word = 
            word
            |> onlyWords.Matches
            |> Seq.cast<Match>
            |> Seq.map (fun m -> m.Value)
        
        let isNGram nGram =
             if (nGram |> Seq.length = n) then Some (words |> String.concat(" "))
             else None

        words
        |> Seq.collect isWord
        |> isNGram

    text.Split(" ")
    |> Seq.windowed n
    |> Seq.choose generateNGram
    |> Seq.toArray

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

type WordScreening = 
    { Word: string
      Score: float 
      Count : int }

let wordCountByLabel = 
    // Word count is done only on the training set
    train
    |> Seq.map (fun xs -> xs.Label, xs.EarningsCall)
    |> Seq.groupBy fst
    |> Seq.map (fun (group, text) -> 
        let wordFreqs = 
            text 
            |> Seq.collect (fun (_, text) -> text |> nGrams 1)
            |> Seq.countBy id
            |> Seq.toArray
        group, wordFreqs
        |> Map)
    |> Seq.toArray
    |> Map

let scoreWord word =

    let countWordInGroup word group = 
        wordCountByLabel.TryFind group
        |> Option.bind (fun wordFreqMap -> wordFreqMap.TryFind word)
        
    let positiveCount, negativeCount = 
        countWordInGroup word Positive, countWordInGroup word Negative

    match positiveCount, negativeCount with
    | Some p, Some n -> 
        let score = ((float p / (float p + float n)))
        let count = p + n
        Some { Word = word
               Score = score
               Count = count}

    | Some p, None -> 
        Some { Word = word
               Score = 1.
               Count = p}

    | None, Some n -> 
        Some { Word = word
               Score = 0.
               Count = n}
    | _ ->  None

let scoreWords (transcripts: LabeledTranscript [])= 
    transcripts
    |> Seq.collect (fun xs -> xs.EarningsCall |> nGrams 1)
    |> Seq.distinct
    |> Seq.choose scoreWord
    |> Seq.toArray

(**
#### 1B) Sentiment-charged set of words

$$\hat{S} = \{j: f_{j} \geq \hat{\pi} + \alpha_{+}, \text{ or } f_{j} \leq \hat{\pi} - \alpha_{-} \} \cap \{ j: k_{j} \geq \kappa\}$$

- $f_{j} = \text{Sentiment-screening score of word } j $
- $\hat{\pi} = \text{Fraction of articles tagged with a positive return}$
- $\alpha_{+} = \text{Upper sentiment-score threshold}$
- $\alpha_{-} = \text{Lower sentiment-score threshold}$
- $k_{j} = \text{count of word } j \text{ in all articles}$

The thresholds ($\alpha{+}, \alpha{-}, \kappa$) are *hyper-parameters* that can be tuned via cross-validation.
*)

/// Fraction of articles tagged with positive returns
    
/// Sentiment-charged set of words
let getSentimentSet alphaUpper alphaLower kappa scoredWords = 
    
    // Fraction of articles tagged with positive returns
    let pieHat = 
        train 
        |> Array.filter (fun xs -> xs.Label = Positive)
        |> fun posObs -> float posObs.Length / float train.Length

    // Screening for sentiment charged words
    scoredWords
    |> Seq.filter (fun xs -> 
        let upperThresh, lowerThresh = (pieHat + alphaUpper), (pieHat - alphaLower)
        (xs.Score >= upperThresh || xs.Score <= lowerThresh) && (xs.Count >= kappa))
    |> Seq.toArray

let sentimentSet = 
    let scoredWords = scoreWords train 
    let alphaUpper, alphaLower, kappa = 0.15, 0.15, 200
    getSentimentSet alphaUpper alphaLower kappa scoredWords

(**
## 2. Learning Sentiment Topics
*)

(**

*)


(**
## 3. Scoring New Articles
*)