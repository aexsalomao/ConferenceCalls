#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json"
#r "nuget: Plotly.NET, 2.0.0-preview.6"
#r "nuget: FSharp.Collections.ParallelSeq"

#load "TranscriptParsing.fsx"
#load "ReturnsAroundCall.fsx" 

open System
open System.Text.RegularExpressions
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
open FSharp.Collections.ParallelSeq

open TranscriptParsing
open ReturnsAroundCall

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
# Transcripts from Motley Fool
*)

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<AnnouncementDayReturn>>(json)

let rawDataset = readJson ("data-cache/ReturnsAroundCall2500.json") |> Seq.toArray

(**
# Analyzing paragraphs
Some paragraphs are longer than others, and some "paragraphs" aren't even real paragraphs. One-liners like opening and closing remarks add noise to our text dataset and should be taken care of. 
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

type LabeledTranscript = 
    {TickerExchange : (string * string)
     Label : Label
     CumulativeReturn: float
     Transcript : string}

let parseLabel (cumRet: float): Label = 
    let threshold = 0.05
    if cumRet > threshold then 
        Positive 
    elif cumRet < -threshold then
        Negative
    else
        Neutral

let removeShortParagraphs (paragraphs: string []): string =
    paragraphs
    |> Seq.filter (fun p -> p.Length > 100)
    |> String.concat (" ")

let labelDataset (dataset: AnnouncementDayReturn []): LabeledTranscript [] = 
    dataset
    |> Seq.map (fun xs ->

        let tickerExchange = (xs.Transcript.Ticker , xs.Transcript.Exchange)
        let label = parseLabel xs.CumulativeReturn
        let transcript = removeShortParagraphs xs.Transcript.Paragraphs

        { TickerExchange = tickerExchange
          Label = label
          CumulativeReturn = xs.CumulativeReturn
          Transcript = transcript})

    |> Seq.toArray

let myTranscripts: LabeledTranscript [] = labelDataset rawDataset

(**
## Analyze dataset
*)

type LabelSummary = 
    { Label : Label
      Count : int
      AvgCumRet : float }

let analyzeLabel (label: Label): LabelSummary = 
    let rets = 
        myTranscripts 
        |> Seq.filter (fun xs -> xs.Label = label)
        |> Seq.map (fun xs -> xs.CumulativeReturn)
        |> Seq.toArray
    
    let count = rets.Length
    let avgCumRet = Math.Round (Array.average rets, 4)
      
    { Label = label
      Count = count
      AvgCumRet = avgCumRet}

(**
## Cumulative returns, histogram
*)

let retsByLabel (label: Label) = 
    myTranscripts
    |> Seq.filter (fun xs -> xs.Label = label)
    |> Seq.map (fun xs -> xs.CumulativeReturn)
    |> Seq.toArray

let returnsDistribution =
    myTranscripts
    |> Seq.map (fun xs -> xs.Label)
    |> Set.ofSeq
    |> Seq.map (fun label -> 
        let labelStats = analyzeLabel label
        Chart.Histogram (data = retsByLabel label, 
                         Name = $"{labelStats.Label} (N = {labelStats.Count}, Avg. = {labelStats.AvgCumRet})"))
    |> Seq.toArray
    |> Chart.Combine
    |> Chart.withTitle "Histogram: Cumulative returns"
    |> Chart.withSize (1000., 500.)

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
        |> Seq.map (fun xs -> (xs.Label, xs.Transcript))
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

let onlyWordsRegex = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")

let tokenizeAllWords (text: string) = 
    text
    |> onlyWordsRegex.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

let applyTokenizer (tokenizer: Tokenizer) =
    training
    |> Seq.map snd
    |> vocabulary tokenizer

(**
# Evaluate by tokenizer and token set
*)

let evaluate (tokenizer: Tokenizer) (tokens: Token Set) = 
    let classifier = train training tokenizer tokens
    validation
    |> Seq.averageBy (fun (label, text) -> 
        if label = classifier text then 1.0 else 0.)
    |> printfn "Correctly classified: %.4f"

// Evaluate tokenizer 1 -> 0.7126 (Cumulative return threshold -> abs 0.05)
let allTokens = applyTokenizer tokenizeAllWords
evaluate tokenizeAllWords allTokens

(**
## N-Grams
*)

let onylWords (word: string): option<Match> = 
    let candidateMatch = onlyWordsRegex.Match word
    if candidateMatch.Success then Some (candidateMatch) else None

let nGrams (n: int) (text: string) = 
    text.Split(" ")
    |> Seq.windowed n
    |> Seq.map (fun words -> 
                words 
                |> Seq.choose onylWords 
                |> Seq.map (fun m -> m.Value) 
                |> Seq.toArray)
    |> Seq.filter (fun nGram -> nGram.Length = n)
    |> Seq.map (String.concat(" "))
    |> Seq.toArray

let twoGramsTokenizer (text: string) = 
    text 
    |> nGrams 1
    |> Set.ofArray

let threeGramsTokenizer (text: string) = 
    text
    |> nGrams 3
    |> Set.ofArray

let woodpecker = 
    "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."

let twoGramsTest = twoGramsTokenizer woodpecker
let threeGramsTest = threeGramsTokenizer woodpecker

// Evaluate TwoGrams -> 0.7480 (Cumulative return threshold -> abs 0.5)
let twoGramsTrain = applyTokenizer twoGramsTokenizer
evaluate twoGramsTokenizer twoGramsTrain

// Evaluate ThreeGrams -> 0.7559 (Cumulative return threshold -> abs 0.5)
let threeGramsTrain = applyTokenizer threeGramsTokenizer
evaluate threeGramsTokenizer threeGramsTrain

(**
- Machine Learning notes/slides (Sabina)

# Feature Selection

To reduce the number of features to something manageable, a common first step is to strip out elements of the raw text other than words.
    - Punctuation, numbers, HTML tags, proper names ...
    - *stop words*: "the", "a", "and", "or" ...

Very rare words do convey meaning, but their added computational cost in expanding 
the set of features that must be considered often exceeds their diagnostic value.

An approach that excludes both common and rare words and has proven very useful in practice 
is filtering by "term frequency-inverse document frequency" (tf-idf).
*)

(**
## Term Frequency (TF)
$tf_{t,d} = \frac{n_{t,d}}{number of terms in a document}$
$n_{t,d}$ : number of times a term *t* is present in a document *d*.
*)

type DocTransfromer = string -> string []

type TermFreq = 
    {Term : string
     Tf : float}

let tf (doc: string) (docTransformer: DocTransfromer): TermFreq [] =
    
    let tokenizedDoc = docTransformer doc
    let nTerms = float tokenizedDoc.Length
   
    tokenizedDoc
    |> Seq.countBy id
    |> Seq.map (fun (term, count) ->
                let tf = (float count) / nTerms
                {Term = term
                 Tf = tf})
    |> Seq.toArray


(**
## Inverse document frequency (IDF)
$IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})
*)

type InverseDocFreq = 
    {Term : string
     Idf : float}

let idf (docs : string []) (docTransfromer: DocTransfromer): Map<string, InverseDocFreq>= 

    let numberOfDocsByTerm = 
            docs
            |> Seq.map docTransfromer
            |> Seq.collect Seq.distinct
            |> Seq.countBy id
            |> Seq.toArray

    let n = docs.Length

    numberOfDocsByTerm
    |> Seq.map (fun (term, docsWithTerm) ->
                let idf = log (float n / float docsWithTerm)
                term, {Term = term ; Idf = idf})
    |> Seq.toArray
    |> Map.ofArray

(**
## Term frequency - Inverse Document Frequency
$TF-IDF(t, d) = TF(t, d) * IDF(t)$
*)

type TermFreqInverseDocFreq = 
    { Term : string
      TfIdf : float}

let tfIdf (doc: string)
          (docTransformer : DocTransfromer)
          (inverseDocFreq: Map<string, InverseDocFreq>) = 
   
   let docTermFrequency = tf doc docTransformer
   
   docTermFrequency
   |> Seq.choose (fun xs -> 
    match Map.tryFind xs.Term inverseDocFreq with
    | None -> None
    | Some ys -> let tfIdf = xs.Tf * ys.Idf 
                 Some ({Term = xs.Term
                        TfIdf = tfIdf}))
    |> Seq.toArray

// Test docs
let doc1 = snd training.[0]
let docs1 = training |> Seq.take 100 |> Seq.map snd |> Seq.toArray

/// DocTransformers
let oneGram: DocTransfromer = nGrams 1
let twoGrams: DocTransfromer = nGrams 2
let threeGrams: DocTransfromer = nGrams 3

/// Tf test
tf doc1 oneGram

/// Idf test
let myMap1 = idf docs1 oneGram
let myMap2 = idf docs1 twoGrams
let myMap3 = idf docs1 threeGrams

// Tf-Idf test
let tfIdfDoc1 = tfIdf doc1 oneGram myMap1
let tfIdfDoc2 = tfIdf doc1 twoGrams myMap2
let tfIdfDoc3 = tfIdf doc1 threeGrams myMap3

(**
# Word bar chart
*)

/// Testing on entire sample
let allDocs = training |> Seq.map snd |> Seq.toArray
let myIdfMap = idf allDocs twoGrams

let tfIdfDoc = tfIdf doc1 twoGrams myIdfMap

let lowTfIdfWords = 
    tfIdfDoc
    |> Seq.sortBy (fun xs -> xs.TfIdf)
    |> Seq.take 25
    |> Seq.toArray

let highTfIdfWords = 
    tfIdfDoc 
    |> Seq.sortByDescending (fun xs -> xs.TfIdf)
    |> Seq.take 25
    |> Seq.toArray

let wordBarChart (words : TermFreqInverseDocFreq []) (title: string) = 
    words
    |> Seq.map (fun xs -> xs.Term, xs.TfIdf)
    |> Seq.toArray
    |> Chart.Bar
    |> Chart.withTitle title
    |> Chart.withSize (1000., 500.)

wordBarChart lowTfIdfWords "Low Tf-Idf words (Common and rare words)" |> Chart.Show
wordBarChart highTfIdfWords "High Tf-Idf words (Relevant words)" |> Chart.Show

(**
**Inverse document frequency (IDF)**
Term *frequency* measures how prevalent a term is in a single document.

But how common it is in the *entire* corpus we're mining?

- A term should not be too *rare*
- A term should not be too *common*

The spareness of a term *t* is measured commonly by its **inverse document frequency**:

- $IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})

*)

(**
**Term Frequency Inverse document frequency TF-IDF**

TF-IDF value is specific to a single document (*d*) whereas IDF depends on the entire corpus.

$TF-IDF(t, d) = TF(t, d) * IDF(t)$

Each document thus becomes a feature vector, and the corpus is the set of these feature vectors.
This set can then be used in a data mining algorithm (naive bayes) for classification, clustering, or retrieval.

A high weight in tf–idf is reached by a high term frequency (in the given document) and a low document frequency of the term in the whole collection of documents; 
the weights hence tend to filter out common terms.

*)

(**
# To-do
- TfIdf by NGrams Tokenizer
*)