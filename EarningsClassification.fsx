(**
# Classifying earnings call transcripts with Naive Bayes
*)

(**
## Import packages and load scripts
*)

#r "nuget: FSharp.Collections.ParallelSeq"

#load "TranscriptParsing.fsx"
#load "ReturnsAroundEarnings.fsx" 

open System
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET

open TranscriptParsing
open ReturnsAroundEarnings

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
## Read transcripts from json file
*)

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<AnnouncementDayReturn>>(json)

let rawDataset = 
    readJson ("data-cache/ReturnsAroundEarningsFullSample.json")
    |> Seq.take 2500
    |> Seq.toArray

(**
# Analyzing paragraphs
*)

(**
Some paragraphs are longer than others, and some paragraphs aren't even real paragraphs. 
One-liners like opening and closing remarks add noise to our dataset and should be taken care of.
*)

let shortParagraphs = 
    rawDataset
    |> Array.collect (fun xs -> 
        xs.Transcript.Paragraphs 
        |> Array.filter (fun paragraph -> paragraph.Length < 100))

shortParagraphs
|> Array.take 10
|> Array.iter (printfn "%s")

let histogramParagraphLength = 
    rawDataset
    |> Seq.collect (fun document -> 
        document.Transcript.Paragraphs
        |> Seq.map (fun paragraph -> float paragraph.Length))
    // Purely for visualization purposes
    |> Seq.filter (fun charCount -> charCount < 1250.)
    |> Seq.toArray
    |> Chart.Histogram
    |> Chart.withX_AxisStyle "Number of characters per paragraph"
    |> Chart.withY_AxisStyle "Frequency"
    |> Chart.withTitle "Histogram: Number of characters per paragraph"
    |> Chart.withSize(500., 500.)

/// Chart.Show histogramParagraphLength
(**
# Labeled Transcript
*)

type Label = 
    | Positive
    | Negative
    | Neutral

let parseLabel cumRet thresh = 
    if cumRet > thresh then Positive 
    elif cumRet < -thresh then Negative
    else Neutral

type LabeledTranscript = 
    { TickerExchange: (string * string) 
      EarningsCall: string
      CumulativeReturn: float} with 
      member this.Label = 
        parseLabel this.CumulativeReturn 0.05

let labelDataset (dataset: AnnouncementDayReturn []): LabeledTranscript [] = 
    let labelTranscript (xs: AnnouncementDayReturn) = 
        let tickerExchange = 
            xs.Transcript.Ticker, xs.Transcript.Exchange
        
        let earningsCall = 
            xs.Transcript.Paragraphs
            |> Seq.filter (fun p -> p.Length > 100)
            |> String.concat (" ")

        { TickerExchange = tickerExchange
          EarningsCall = earningsCall
          CumulativeReturn = xs.CumulativeReturn}
    
    Array.map labelTranscript dataset

let myTranscripts = 
    labelDataset rawDataset

(**
## Analyze dataset
*)

type LabelSummary = 
    { Label: Label
      Count: int
      AvgCumRet: float }

let analyzeDatasetByLabel (dataset: LabeledTranscript []) (label: Label): LabelSummary =     
    let getRets label = 
        dataset 
        |> Seq.filter (fun xs -> xs.Label = label)
        |> Seq.map (fun xs -> xs.CumulativeReturn)
        |> Seq.toArray
    
    let rets = getRets label
    let count = rets.Length
    let avgCumRet = Array.average rets |> fun x -> Math.Round (x, 4)
    
    { Label = label
      Count = count
      AvgCumRet = avgCumRet }

myTranscripts
|> Seq.map (fun xs -> xs.Label)
|> Seq.distinct
|> Seq.map (analyzeDatasetByLabel myTranscripts)
|> Seq.toArray

(**
## Cumulative returns, histogram
*)

let retsByLabel (label: Label) = 
    myTranscripts
    |> Seq.filter (fun xs -> xs.Label = label)
    |> Seq.map (fun xs -> xs.CumulativeReturn)
    |> Seq.toArray  

/// Chart.Show returnsDistribution
(**
# Naive Bayes Module
*)

type Token = string
type Tokenizer = string -> Token Set
type TokenizedDoc = Token Set

type DocGroup = 
    { Proportion : float
      TokenFrequencies : Map<Token, float> }

let tokenScore (group: DocGroup) (token: Token) =
    if group.TokenFrequencies.ContainsKey token
    then log group.TokenFrequencies.[token]
    else 0.0

let score (document: TokenizedDoc) (group: DocGroup) = 
    let scoreToken = tokenScore group
    log group.Proportion + 
    (document |> Seq.sumBy scoreToken)

let classify (labelGroups: ('Label * DocGroup)[])
             (tokenizer: Tokenizer)
             (txt: string) = 
    let tokenized = tokenizer txt
    labelGroups
    |> Array.maxBy (fun (label, group) -> 
        score tokenized group)
    |> fst

let proportion count total = float count / float total

let laplace count total = float (count+1) / float (total+1)

let countIn (docs: TokenizedDoc seq) (token: Token) =
    docs
    |> Seq.filter (Set.contains token)
    |> Seq.length

let analyze (docsThisLabel: TokenizedDoc seq)
            (nTotalDocs: int)
            (vocabulary: Token Set) =
    let nThisLabel = docsThisLabel |> Seq.length
    let score token =
        let count = countIn docsThisLabel token
        laplace count nThisLabel
    let scoredTokens =
        vocabulary
        |> Set.map (fun token -> token, score token)
        |> Map.ofSeq
    let labelProportion = proportion nThisLabel nTotalDocs

    { Proportion = labelProportion 
      TokenFrequencies = scoredTokens }

let learn (docs: ('Label * string)[])
          (tokenizer: Tokenizer)
          (vocabulary: Token Set) =
    let total = docs.Length
    docs
    |> PSeq.map (fun (label, docString) -> label, tokenizer docString)
    |> PSeq.groupBy fst
    |> PSeq.map (fun (label, (xs: seq<'Label * TokenizedDoc>)) -> 
        let tokenizedDocs = xs |> Seq.map snd
        label, analyze tokenizedDocs total vocabulary)
    |> PSeq.toArray

let train (docs: ('Label * string)[]) 
          (tokenizer: Tokenizer)
          (vocabulary: Token Set) =
    let labelGroups = learn docs tokenizer vocabulary
    let classifier = classify labelGroups tokenizer
    classifier

(**
## Dataset split
*)

let training, validation =
    let posOrNeg = 
        myTranscripts
        |> Seq.filter (fun xs -> xs.Label <> Neutral)
        |> Seq.map (fun xs -> (xs.Label, xs.EarningsCall))
        |> Seq.toArray

    let cutoff = (float posOrNeg.Length) * 0.8 |> int

    posOrNeg.[.. cutoff], posOrNeg.[cutoff + 1 ..]

(**
## Tokenizer
*)

let vocabulary (tokenizer: Tokenizer) (corpus: string seq) = 
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

let onlyWords = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")

let tokenizeAllWords (text: string) = 
    text
    |> onlyWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let applyTokenizer (tokenizer: Tokenizer) =
    training
    |> Seq.map snd
    |> vocabulary tokenizer

(**
## Evaluate performance by tokenizer
*)

let evaluate (tokenizer: Tokenizer) (tokens: Token Set) = 
    let classifier = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (label, text) -> 
        if label = classifier text then 1.0 
        else 0.)
    |> printfn "Correctly classified: %.4f"

(**
## N-Grams (DocTransformer)
*)

let onylWords (word: string): option<Match> = 
    let candidateMatch = onlyWords.Match word
    if candidateMatch.Success then Some (candidateMatch) 
    else None

let nGrams (n: int) (text: string): string [] = 
    let sentences = 
        text.Split(" ")
        |> Seq.windowed n

    let getOnlyWords words =
        words 
        |> Seq.choose onylWords 
        |> Seq.map (fun m -> m.Value) 

    let tryNGram words = 
        if (Seq.length words = n) then Some (String.concat(" ") words)
        else None

    sentences
    |> Seq.map getOnlyWords
    |> Seq.choose tryNGram
    |> Seq.toArray

let twoGramsTokenizer (text: string): Set<string> = 
    nGrams 2 text
    |> Set.ofArray

let threeGramsTokenizer (text: string): Set<string> = 
    nGrams 3 text
    |> Set.ofArray

/// Some tests
let woodpecker = "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."
let twoGramsTest = twoGramsTokenizer woodpecker
let threeGramsTest = threeGramsTokenizer woodpecker

(**
## Term Frequency (TF)
$tf_{t,d} = \frac{n_{t,d}}{number of terms in a document}$
$n_{t,d}$ : number of times a term *t* is present in a document *d*.
*)

type DocTransfromer = string -> string []

type TermFreq = {Term: string; Tf: float}

let tf (doc: string) 
       (docTransformer: DocTransfromer): TermFreq [] =
    let getTf (terms: string []) = 
        let nTerms = terms.Length |> float
        terms
        |> Seq.countBy id
        |> Seq.map (fun (term, count) ->
            let tf = (float count) / (nTerms)
            {Term = term; Tf = tf })
        |> Seq.toArray
    
    (docTransformer doc |> getTf)

(**
## Inverse document frequency (IDF)
$IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})
*)

type InverseDocFreq = {Term: string; Idf: float }

let idf (docs : string []) (docTransfromer: DocTransfromer) = 
    let numberOfDocsByTerm = 
            docs
            |> Seq.map docTransfromer
            |> Seq.collect PSeq.distinct
            |> Seq.countBy id
            
    let getIdf term docsWithTerm =
        let n = docs.Length
        let idf = log (float n / float docsWithTerm)
        term, { Term = term; Idf = idf }

    numberOfDocsByTerm
    |> Seq.map (fun (term, docsWithTerm) -> getIdf term docsWithTerm)
    |> Map

(**
## Term frequency - Inverse Document Frequency
$TF-IDF(t, d) = TF(t, d) * IDF(t)$
A high weight in tf–idf is reached by a high term frequency (in the given document) and a low document frequency of the term in the whole collection of documents; 
the weights hence tend to filter out common terms.
*)

type TermFreqInverseDocFreq = 
    { Term: string
      TfIdf: float }

let tfIdf (doc: string)
          (docTransformer : DocTransfromer)
          (inverseDocFreq: Map<string, InverseDocFreq>) = 
   let getTfIdf (termFreq: TermFreq) =
        match Map.tryFind termFreq.Term inverseDocFreq with
        | None -> None
        | Some inverseTermFreq -> 
            let tfIdf = termFreq.Tf * inverseTermFreq.Idf 
            Some {Term = termFreq.Term; TfIdf = tfIdf}
  
   tf doc docTransformer 
   |> Array.choose getTfIdf

let tfIdfFromDoc (doc: string) 
                 (docTransformer: DocTransfromer)
                 (inverseDocFreq: Map<string, InverseDocFreq>) =

    tfIdf doc docTransformer inverseDocFreq

(**
## Tf-Idf tests
*)

// Test docs
let doc1 = snd training.[0]
let docs1 = training |> Seq.take 100 |> Seq.map snd |> Seq.toArray

/// DocTransformers
let oneGram: DocTransfromer = nGrams 1
let twoGrams: DocTransfromer = nGrams 2
let threeGrams: DocTransfromer = nGrams 3

/// Tf test
tf doc1 oneGram
tf doc1 twoGrams
tf doc1 threeGrams

/// Idf test
let idf1 = idf docs1 oneGram
let idf2 = idf docs1 twoGrams
let idf3 = idf docs1 threeGrams

/// Tf-Idf test
let tfIdfDoc1 = tfIdf doc1 oneGram idf1
let tfIdfDoc2 = tfIdf doc1 twoGrams idf2
let tfIdfDoc3 = tfIdf doc1 threeGrams idf3

/// Full sample test - TwoGrams
let allDocs = training |> Array.map snd
let twoGramsIdf = idf allDocs twoGrams
let threeGramsIdf = idf allDocs threeGrams
let tfIdfDoc1FullSample = tfIdfFromDoc doc1 twoGrams twoGramsIdf

(**
# Word bar chart - Doc1
*)

let lowTfIdfWords = 
    tfIdfDoc1FullSample
    |> Seq.sortBy (fun xs -> xs.TfIdf)
    |> Seq.take 25
    |> Seq.sortByDescending (fun xs -> xs.TfIdf)
    |> Seq.toArray

let highTfIdfWords = 
    tfIdfDoc1FullSample 
    |> Seq.sortByDescending (fun xs -> xs.TfIdf)
    |> Seq.take 25
    |> Seq.toArray

let wordBarChart (words : TermFreqInverseDocFreq []) (title: string) = 
    words
    |> Seq.map (fun xs -> xs.Term, xs.TfIdf)
    |> Seq.toArray
    |> Chart.Column
    |> Chart.withTitle title
    |> Chart.withSize (500., 750.)

wordBarChart lowTfIdfWords "Low Tf-Idf words (Common and rare words)" |> Chart.Show
wordBarChart highTfIdfWords "High Tf-Idf words (Relevant words)" |> Chart.Show

(**
# Tf-idf histogram
*)

/// TwoGrams
let allDocsTwoGramsTfIdf = 
    allDocs
    |> Array.collect (fun doc -> tfIdfFromDoc doc twoGrams twoGramsIdf)

/// ThreeGrams
let allDocsThreeGramsTfIdf = 
    allDocs
    |> Array.collect (fun doc -> tfIdfFromDoc doc threeGrams threeGramsIdf)

let description =
    let heading = "Comments"
    let description = 
        "Very rare words do convey meaning, but their added computational cost in expanding 
         the set of features that must be considered often exceeds their diagnostic value.
         An approach that excludes both common and rare words and has proven very useful in practice 
         is filtering by 'term frequency - inverse document frequency' (tf-idf).
         A high weight in tf-idf is reached by a high term frequency (in the given document) and 
         a low document frequency of the term in the whole collection of documents; 
         the weights hence tend to filter out common terms."

    ChartDescription.create heading description

let tfIdfHistogram (termsWithTfIdf: TermFreqInverseDocFreq [])
                   (description: ChartDescription)
                   (title: string) =
    termsWithTfIdf
    |> Seq.map (fun xs -> xs.TfIdf)
    // For vizualization purposes
    |> Seq.filter (fun x -> x < 0.005)
    |> Seq.toArray
    |> Chart.Histogram
    |> Chart.withTitle title
    |> Chart.withX_AxisStyle "Term frequency - inverse document frequency"
    |> Chart.withY_AxisStyle "Frequency"
    |> Chart.WithDescription (description)
    |> Chart.withSize (600., 600.)

let twoGramsHist = tfIdfHistogram allDocsTwoGramsTfIdf description "2-Grams"
let threeGramsHist = tfIdfHistogram allDocsThreeGramsTfIdf description "3-Grams"

Chart.Show twoGramsHist
Chart.Show threeGramsHist

twoGramsHist |> Chart.SaveHtmlAs "data-cache/2GramsHist"

(**
# Tf-Idf + NGrams Tokenizer
*)

let tfIdfNGramsTokenizer (tfIdfThresh : float)
                         (inverseDocFreq: Map<string, InverseDocFreq>)
                         (docTransformer: DocTransfromer) 
                         (doc: string) = 
    let tdIdfofDoc = 
        tfIdf doc docTransformer inverseDocFreq
        |> Seq.map (fun xs -> xs.Term, xs)
        |> Map
    
    let relevantTerms term = 
        match Map.tryFind term tdIdfofDoc with 
        | Some term when term.TfIdf > tfIdfThresh -> Some term.Term
        | _ -> None
    
    doc
    |> docTransformer
    |> Seq.distinct
    |> Seq.choose relevantTerms
    |> Set

(**
# Tokenizer evaluation
## Simple Tokenizer (Regex single word)
*)

/// Evaluate simple tokenizer -> 0.7126 (Cumulative return threshold -> abs 0.05)
let allTokens = applyTokenizer tokenizeAllWords
evaluate tokenizeAllWords allTokens

(**
## NGrams Tokenizer
*)

/// Evaluate TwoGrams -> 0.7480 (Cumulative return threshold -> abs 0.5)
let twoGramsTokens = applyTokenizer twoGramsTokenizer
evaluate twoGramsTokenizer twoGramsTokens

/// Evaluate ThreeGrams -> 0.7559 (Cumulative return threshold -> abs 0.5)
let threeGramsTokens = applyTokenizer threeGramsTokenizer
evaluate threeGramsTokenizer threeGramsTokens

(**
# Tf-Idf + NGrams Tokenizer
- Threshold -> Hyperparameter ? 
*)

/// Evaluate TfIdf + twoGrams Tokenizer ->  0.7756 (Cumulative return threshold -> abs 0.5, TfIdfThesh = 0.0005)
let twoGramsTransformer = nGrams 2
let trainingTwoGramsIdf = idf allDocs twoGramsTransformer
let tfIdfTwoGramsTokenzier = tfIdfNGramsTokenizer 0.0005 trainingTwoGramsIdf twoGramsTransformer

let tfIdfTwoGramsTokens = applyTokenizer tfIdfTwoGramsTokenzier
evaluate tfIdfTwoGramsTokenzier tfIdfTwoGramsTokens

/// Evaluate TfIdf + threeGrams Tokenizer ->  0.6923 (Cumulative return threshold -> abs 0.5, TfIdfThresh = 0.0008)
let threeGramsTransformer = nGrams 3
let trainingThreeGramsIdf = idf allDocs threeGramsTransformer
let tfIdfThreeGramsTokenzier = tfIdfNGramsTokenizer 0.001 trainingThreeGramsIdf threeGramsTransformer

let tfIdfThreeGramsTokens = applyTokenizer tfIdfThreeGramsTokenzier
evaluate tfIdfThreeGramsTokenzier tfIdfThreeGramsTokens

(**
## Positive, Negative words analysis
*)