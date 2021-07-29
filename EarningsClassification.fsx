#r "nuget: FSharp.Data"
#r "nuget: Newtonsoft.Json, 13.0.1"
#r "nuget: Plotly.NET, 2.0.0-preview.6"

open System
open System.Text.RegularExpressions
open FSharp.Data
open Newtonsoft.Json
open Plotly.NET

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
fsi.AddPrinter<DateTime>(fun dt -> dt.ToString("s"))

(**
# Transcripts from Motley Fool
*)

// Types
type Transcript = 
    {Ticker : string
     Exchange: string
     Date : DateTime
     Paragraphs : string}

type AnnouncementDayReturn = 
    { Transcript : Transcript
      CumulativeReturn : float}

let readJson (jsonFile: string) =
    IO.File.ReadAllText(jsonFile)
    |> fun json -> JsonConvert.DeserializeObject<seq<AnnouncementDayReturn>>(json)

let datasetRaw = readJson ("data-cache/AnnouncementDay1950.json")

(**
# Stop words
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
    
    let positiveThresh = cumRet >= 0.075
    let negativeThresh = cumRet <= -0.075
    
    match cumRet with
    | _ when positiveThresh  -> Positive
    | _ when negativeThresh -> Negative
    | _ -> Neutral

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
    |> Array.map (fun (label, docString) -> label, tokenizer docString)
    |> Seq.groupBy fst
    |> Seq.map (fun (label, (xs: seq<'Label * TokenizedDoc>)) -> 
        let tokenizedDocs = xs |> Seq.map snd
        label, analyze tokenizedDocs total vocabulary)
    |> Seq.toArray

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
let training, validation = onlyPositiveOrNegative.[.. 599], onlyPositiveOrNegative.[600 ..]

(**
## Tokenizers
*)

// Word Tokenizer 1
let matchOnlyWords = Regex(@"\w+")

let wordTokenizerAllWordsLowerCased (text: string) = 
    text.ToLowerInvariant()
    |> matchOnlyWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

// Word Tokenizer 2
let wordTokenizerAllWords (text: string) = 
    text
    |> matchOnlyWords.Matches
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Value)
    |> Set.ofSeq

// Word Tokenizer 3
let wordTokenizerStopWords (text: string) = 
    text
    |> matchOnlyWords.Matches
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

// Evaluate tokenizer 1 --> 0.6732
let allTokensLowerCased = applyTokenizer wordTokenizerAllWordsLowerCased
evaluate wordTokenizerAllWordsLowerCased allTokensLowerCased

// Evaluate tokenizer 2 --> 0.6863
let allTokens = applyTokenizer wordTokenizerAllWords
evaluate wordTokenizerAllWords allTokens

// Evaluate tokenizer 3 --> 0.6863 ??
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

// Evaluate allTopTokens --> (Seq.sortByDescending) 0.6405
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

// Evaluate specificTopTokens --> (Seq.sortByDescending) 0.6536
// Evaluate specificTopTokens --> (Seq.sortBy) 0.5817 Typo in book ?
evaluate wordTokenizerAllWords specificTopTokens

(**
## N-Grams

- TO-DO: 
    - Add StopWords functionality to N-Grams
    - Some of the grams look wrong, they have double quotation marks 
*)

let woodpecker = 
    "The cream-colored woodpecker (Celeus flavus) is unmistakably recognizable by its pale but distinct yellow plumage and beak, long erect crest, dark brown wings and black tail. The male is differentiated by the female by its thick bright red malar stripe. The yellow plumage may darken to a browner or darker tone if soiled. The cream-colored woodpecker is 24–26 centimetres (9.4–10.2 in) in height and weighs 95–130 grams (3.4–4.6 oz)."

let simpleNGrams (n: int) (text: string) = 
    text.Split(" ")
    |> Seq.windowed n
    |> Seq.map (fun nGramsArr -> 
                nGramsArr 
                |> String.concat(" ") 
                |> fun nGrams -> nGrams.Trim())
    |> Set.ofSeq

let TwoGramsTokenizer = simpleNGrams 2
let ThreeGramsTokenizer = simpleNGrams 3

let twoGramsTest = TwoGramsTokenizer woodpecker
let threeGramsTest = ThreeGramsTokenizer woodpecker

// Evaluate TwoGramsTokenizer -> 0.6667
let twoGrams = applyTokenizer TwoGramsTokenizer
evaluate TwoGramsTokenizer twoGrams

// Evaluate ThreeGramsTokenizer --> Did not eval. :/ 
let threeGrams = applyTokenizer ThreeGramsTokenizer
evaluate TwoGramsTokenizer threeGrams
