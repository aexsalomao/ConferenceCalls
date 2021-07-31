#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json, 13.0.1"
#r "nuget: Plotly.NET, 2.0.0-preview.6"
#r "nuget: FSharp.Collections.ParallelSeq"

#load "PagePParsing.fsx"
open PagePParsing

open System
open System.Text.RegularExpressions
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET
open FSharp.Collections.ParallelSeq

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
# Transcripts from Motley Fool
*)

// Types
type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float}

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<AnnouncementDayReturn>>(json)

let datasetRaw = readJson ("data-cache/AnnouncementDay1950.json")

(**
# Stop words

type StopWordsCsv = CsvProvider<Sample="data-cache/StopWords.csv",
                                ResolutionFolder= __SOURCE_DIRECTORY__>

let myStopWordsCsv = StopWordsCsv.Load(__SOURCE_DIRECTORY__ + "/data-cache/StopWords.csv")

let stopWordsArr = 
    myStopWordsCsv.Rows
    |> Seq.map (fun x -> x.DefaultStopWords)
    |> Seq.toArray
*)

let [<Literal>] StopWordsFilePath = "C:\Users\Five star\Documents\GitHub\ConferenceCalls\data-cache\StopWords.csv"
type StopWordsCsv = CsvProvider<StopWordsFilePath>

let stopWordsArr =
    StopWordsCsv.GetSample()
    |> fun doc -> doc.Rows
    |> Seq.map (fun x -> x.DefaultStopWords)
    |> Seq.toArray

(**
# Label type
*)

type Label = 
    | Positive
    | Negative
    | Neutral

type DatasetOverview = 
    { LabelCount : (Label * int)[]
      PositiveAvg : float
      NegativeAvg : float
      NeutralAvg : float}

let parseLabel (cumRet: float): Label = 
    let threshold = 0.05
    if cumRet > threshold then 
        Positive 
    elif cumRet < -threshold then
        Negative
    else
        Neutral

let analyzeDataset (dt: (Label * AnnouncementDayReturn) []): DatasetOverview = 
    let labelCount = 
        dt
        |> Seq.countBy fst
        |> Seq.toArray
    
    let avgRetByLabel (label: Label): float = 
        dt
        |> Seq.filter (fun (l, _) -> l = label)
        |> Seq.averageBy (fun (_, t) -> t.CumulativeReturn)

    let positiveAvg = avgRetByLabel Positive
    let negativeAvg = avgRetByLabel Negative
    let neutralAvg = avgRetByLabel Neutral

    { LabelCount = labelCount
      PositiveAvg = positiveAvg
      NegativeAvg = negativeAvg
      NeutralAvg = neutralAvg }

// Analyze and label Dataset
let labeledDataset = 
    datasetRaw
    |> Seq.map (fun xs -> (xs.CumulativeReturn |> parseLabel), (xs))
    |> Seq.toArray

analyzeDataset labeledDataset

(**
# Using words as clues
*)

type ClueOverview = 
    {Clue : string
     PositiveCount : int
     NegativeCount : int
     NeutralCount : int}

let clueCountbyLabel (clue: string) (label: Label) : int = 
    labeledDataset
    |> Seq.filter (fun (l, xs) -> l = label && xs.Transcript.Paragraphs.Contains(clue))
    |> Seq.length

let analyzeClue (clue: string) : ClueOverview =
    
    let positiveCount = clueCountbyLabel clue Positive
    let negativeCount = clueCountbyLabel clue Negative
    let neutralCount = clueCountbyLabel clue Neutral

    { Clue = clue
      PositiveCount = positiveCount
      NegativeCount = negativeCount
      NeutralCount = neutralCount}

// Analyzing key words
analyzeClue "loss"
analyzeClue "gain"

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

/// This counts the frequencies of tokens in a document.
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
# Vocabulary, Dataset split
*)

let vocabulary (tokenizer: Tokenizer) (corpus: string seq) = 
    corpus
    |> Seq.map tokenizer
    |> Set.unionMany

let onlyPositiveOrNegative = 
    labeledDataset 
    |> Seq.filter (fun (l, _) -> l <> Neutral)
    |> Seq.map (fun (l, xs) -> (l, xs.Transcript.Paragraphs))
    |> Seq.toArray

let datasetCount = onlyPositiveOrNegative |> Seq.length
let training, validation = onlyPositiveOrNegative.[.. 1500], onlyPositiveOrNegative.[1501 ..]

(**
## Tokenizers
*)

// Word Tokenizer 1
let matchOnlyWords = Regex(@"\w+")
let matchOnlyWords2 = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")

let a = 
    let somePhrase = "I like to test this function"
    somePhrase |> matchOnlyWords.Matches |> Seq.cast<Match> |> Seq.map (fun x -> x.Value) |> Seq.toArray

let wordTokenizerAllWordsLowerCased (text: string) = 
    text.ToLowerInvariant()
    |> matchOnlyWords2.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

// Word Tokenizer 2
let wordTokenizerAllWords (text: string) = 
    text
    |> matchOnlyWords2.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

// Word Tokenizer 3
let wordTokenizerStopWords (text: string) = 
    text
    |> matchOnlyWords2.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq
    |> Set.filter (fun text -> not (stopWordsArr |> Array.contains text))

// Apply (generic) tokenizer
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

// Evaluate tokenizer 1 -> 0.6732 (Cumulative return threshold -> abs 0.075)
// Evaluate tokenizer 1 -> 0.6960 (Cumulative return threshold -> abs 0.05)
let allTokensLowerCased = applyTokenizer wordTokenizerAllWordsLowerCased
evaluate wordTokenizerAllWordsLowerCased allTokensLowerCased

// Evaluate tokenizer 2 -> 0.6863 (Cumulative return threshold -> abs 0.075)
// Evaluate tokenizer 2 -> 0.6960 (Cumulative return threshold -> abs 0.05)
let allTokens = applyTokenizer wordTokenizerAllWords
evaluate wordTokenizerAllWords allTokens

// Evaluate tokenizer 3 -> 0.6863 (Cumulative return threshold -> abs 0.075)
// Evaluate tokenizer 3 -> 0.6920 (Cumulative return threshold -> abs 0.05)
let tokensWithoutStopWords = applyTokenizer wordTokenizerStopWords
evaluate wordTokenizerStopWords tokensWithoutStopWords

(**
# Less is more
- Picking the most frequently found tokens in each document group
- Extract and count how many tokens we have in each group (Vocabulary; set)
- Merge them into one single set.
*)

let top (n: int) (tokenizer: Tokenizer) (docs: string []) = 
    let tokenized = docs |> Array.map tokenizer
    let tokens = tokenized |> Set.unionMany

    tokens
    |> Seq.sortByDescending (fun t -> countIn tokenized t)
    |> Seq.take n
    |> Set.ofSeq

let allPositiveText = 
    training 
    |> Seq.filter (fun (label, _) -> label=Positive)
    |> Seq.map (fun (_, txt) -> txt)
    |> Seq.toArray

let allNegativeText = 
    training
    |> Seq.filter (fun (label, _) -> label=Negative)
    |> Seq.map (fun (_, txt) -> txt)
    |> Seq.toArray

// WITHOUT StopWords ! --> Since we are already filtering out for the stop words, we are really getting the top common words that are not stop words.

let positiveCount = allPositiveText |> vocabulary wordTokenizerStopWords |> Set.count
let negativeCount = allNegativeText |> vocabulary wordTokenizerStopWords |> Set.count

let topPositiveTokens = allPositiveText |> top (positiveCount / 10) wordTokenizerStopWords
let topNegativeTokens = allNegativeText |> top (negativeCount / 10) wordTokenizerStopWords

let allTopTokens = Set.union topPositiveTokens topNegativeTokens

// Evaluate allTopTokens --> (Seq.sortByDescending) 0.6840
// Evaluate allTopTokens --> (Seq.sortBy) 0.5882 -> Typo in book ?
evaluate wordTokenizerStopWords allTopTokens

(**
# Choosing our words more carefully 
- Removing stop words
- Instead of relying on a list of stop words, we can assume that most stop words can be found in the intersection between the two sets.
- By keeping only Positive and Negative specific words we should get superior results.
*)

// "Outer-join"
let commonTopTokens = Set.intersect topPositiveTokens topNegativeTokens
let specificTopTokens = Set.difference allTopTokens commonTopTokens

// Evaluate specificTopTokens --> (Seq.sortByDescending) 0.6840
// Evaluate specificTopTokens --> (Seq.sortBy) 0.5817 Typo in book ?
evaluate wordTokenizerAllWords specificTopTokens

(**
## N-Grams

- TO-DO: 
    - Improve regex expression
*)

let woodpecker = 
    "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."

// New Regex Expression
// (https://stackoverflow.com/questions/62383043/get-only-words-start-and-end-with-either-letters-or-digits-using-regular-express)

let isOnlyWords = Regex(@"(?<!\S)[a-zA-Z0-9]\S*[a-zA-Z0-9](?!\S)")

let trySingleWordMatch (word: string) = 
    word
    |> isOnlyWords.Matches
    |> Seq.tryExactlyOne

let completeNGram (n: int) (nGramArr: string []) = 
    nGramArr
    |> Seq.choose trySingleWordMatch
    |> Seq.map (fun matchedWord -> matchedWord.Value)
    |> fun xs -> if (xs |> Seq.length) = n then Some (xs) else None

let nGrams (n: int) (text: string) = 
    text.Split(" ")
    |> Seq.windowed n
    |> Seq.choose (n |> completeNGram)
    |> Seq.map (String.concat(" "))
    |> Set.ofSeq

let TwoGrams = nGrams 2
let twoGramsTest = woodpecker |> TwoGrams

// Evaluate TwoGrams -> 0.7080 (Cumulative return threshold -> abs 0.5)
let twoGrams = applyTokenizer TwoGrams

twoGrams |> Seq.take 100 |> Seq.iter (printfn "%s")

evaluate TwoGrams twoGrams

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
**Term frequency (TF)**

Instead of just representing presence or absence of a word, we count words (calculate frequency) in the document.

$tf_{t,d} = \frac{n_{t,d}}{number of terms in a document}$

$n_{t,d}$ : number of times a term *t* is present in a document *d*.
*)

// Term Frequency (TF)
let DocTerms (doc: string) = 
    doc.Split(" ")
    |> Seq.choose trySingleWordMatch
    |> Seq.map (fun matchedWord -> matchedWord.Value)

let NumberOfTermsInDoc (docTerms: string []) = 
    docTerms
    |> Seq.length
    |> float

// This is the term frequency (TF)
let docTermFrequency (docTerms: string []) =
    docTerms 
    |> Array.countBy id

/// This is the inverse document frequency (IDF)
let numberOfDocsContainingTerms (docs: string [] []) =
    let numberOfDocsByTerm = 
        docs
        |> Array.collect Array.distinct
        |> Array.countBy id
    let n = docs.Length
    numberOfDocsByTerm
    |> Array.map(fun (term, docsWithTerm) ->
        term, log(float n / float docsWithTerm))
    |> Map

let tfidf (doc: string []) (idf: Map<string, float>) =
    doc
    |> docTermFrequency
    |> Array.map (fun (term, tf) ->
        term, float tf * idf.[term])















let inverseDocFrequency (docs)

let termFrequencies (docTerms: string []) =
    let termCounts = docTerms |> Array.countBy id 
    let numberOfTermsInDoc =
        termCounts
        |> Seq.sumBy snd
        |> float
    termCounts
    |> Array.map(fun (term, count) -> 
        (term, float count / numberOfTermsInDoc ))            
    |> Map

let TermCountInDoc (docTerms: string []) (term: string) = 
    docTerms
    |> Seq.filter (fun aTerm -> aTerm = term)
    |> Seq.length
    |> float

let TermFrequency (doc: string) (term: string) = 
    let docTerms = DocTerms doc |> Seq.toArray
    
    (TermCountInDoc docTerms term) / (NumberOfTermsInDoc docTerms)

// Sample Document and Term
let documentD = 
    training
    |> Seq.head
    |> snd

let termT = "the"

// TermFrequency test
let tfTermT = TermFrequency documentD termT

(**
**Inverse document frequency (IDF)**
Term *frequency* measures how prevalent a term is in a single document.

But how common it is in the *entire* corpus we're mining?

- A term should not be too *rare*
- A term should not be too *common*

The spareness of a term *t* is measured commonly by its **inverse document frequency**:

- $IDF (t) = 1 + log(\frac{Total number of documents}{Number of documents containing *t*})

*)

// Inverse Document Frequency
let NumberOfDocsWithTerm (term: string) = 
    training
    |> Seq.sumBy (fun (_, text) -> if text.Contains(term) then 1. else 0.)

let InverseDocumentFrequency (term: string) = 
    let documentCount = training |> Seq.length |> float
    1. + log10 ((documentCount / (NumberOfDocsWithTerm term)))

// Test InverseDocumentFrequency
let idfTermT = InverseDocumentFrequency termT

(**
**Term Frequency Inverse document frequency TF-IDF**

TF-IDF value is specific to a single document (*d*) whereas IDF depends on the entire corpus.

$TF-IDF(t, d) = TF(t, d) * IDF(t)$

Each document thus becomes a feature vector, and the corpus is the set of these feature vectors.
This set can then be used in a data mining algorithm (naive bayes) for classification, clustering, or retrieval.

A high weight in tf–idf is reached by a high term frequency (in the given document) and a low document frequency of the term in the whole collection of documents; 
the weights hence tend to filter out common terms.

*)

// Term frequency - inverse document frequency
let TfIdf (doc: string) (term: string) =
    (TermFrequency doc term) * (InverseDocumentFrequency term)

// Test TF-IDF
TfIdf documentD termT

// TF-IDF Type ("Feature vector")
type TfIdfByTerm = 
    {Term : Token
     TfIdf : float}

let TfIdfByDoc (doc: string)=
    doc
    |> DocTerms
    |> Set.ofSeq
    |> Seq.map (fun term -> 
        let tfIdf = TfIdf documentD term
        { Term = term
          TfIdf = tfIdf})
    |> Seq.toArray

// TfIdfByDoc test
let documentDFeatures = TfIdfByDoc documentD

let commonTerms = documentDFeatures |> Seq.sortByDescending (fun xs -> xs.TfIdf) |> Seq.take 30 |> Seq.toArray
let rareTerms = documentDFeatures |> Seq.sortBy (fun xs -> xs.TfIdf) |> Seq.take 30 |> Seq.toArray

let wordBarChart (terms: TfIdfByTerm []) (title: string) = 
    terms
    |> Seq.map (fun xs -> xs.Term, xs.TfIdf)
    |> Chart.Bar
    |> Chart.withTitle title
    |> Chart.withX_AxisStyle "Term Frequency - Inverse Document Frequency"

wordBarChart commonTerms "Common Terms" |> Chart.Show
wordBarChart rareTerms "Rare terms" |> Chart.Show

(**
# To-do
- Combine TfIdf with some tokenizer
- Build Porters algorithm ? 

- Pipeline:
    1. Training data
    2. bi-grams (with regex)
    3. (Porters algorithm ?)
    4. TfIdf filter (threshold -> hyperparameter ?)
*)
