namespace Preprocessing

module Normalization = 

    open System.Text.RegularExpressions

    /// Check for *only* words (Regex)       
    let getOnlyWords (text: string): string= 
        let onlyWords = Regex(@"(?<!\S)[a-zA-Z]\S*[a-zA-Z](?!\S)")

        text.Replace(",", "")
            .Replace(";", "")
            .Replace(":", "")
            .Replace(".", "")
            .Replace("?", "")
            .Replace("!", "")
            .ToLowerInvariant()
        |> onlyWords.Matches
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value)
        |> String.concat(" ")

    /// Non-exhaustive map of english contractions
    let englishContractions: Map<string, string>= 
        [
        ("aren't", "are not")
        ("can't", "cannot")
        ("could've", "could have")
        ("couldn't", "could not")
        ("dammit", "damn it")
        ("didn't", "did not")
        ("doesn't", "does not")
        ("don't", "do not")
        ("dunno", "do not know")
        ("everybody's", "everybody is")
        ("everyone's", "everyone is")
        ("gimme", "give me")
        ("gonna", "going to")
        ("gotta", "got to")
        ("hadn't", "had not")
        ("had've", "had have")
        ("hasn't", "has not")
        ("haven't", "have not")
        ("here's", "here is")
        ("how'll", "how will")
        ("how're", "how are")
        ("i'll", "I will")
        ("i'm", "I am")
        ("imma", "I am about to")
        ("innit", "is it not")
        ("i've", "I have")
        ("isn't", "is not")
        ("it'd",  "it would")
        ("kinda", "kind of")
        ("let's", "let us")
        ("ma'am", "madam")
        ("mayn't", "may not")
        ("may've", "may have")
        ("methinks", "I think")
        ("mightn't", "might not")
        ("might've", "might have")
        ("mustn't", "must not")
        ("mustn't've", "must not have")
        ("must've", "must have")
        ("needn't", "need not")
        ("shan't", "shall not")
        ("should've", "should have")
        ("shouldn't", "should not")
        ("shouldn't've", "should not have")
        ("that're", "that are")
        ("there're", "there are")
        ("these're", "these are")
        ("these've", "these have")
        ("they'll", "they will")
        ("they've", "they have")
        ("they're", "they are")
        ("those're", "those are")
        ("those've", "those have")
        ("wanna", "want to")
        ("wasn't", "was not")
        ("we'd've", "we would have")
        ("we'll", "we will")
        ("we're", "we are")
        ("we've", "we have")
        ("weren't", "were not")
        ("what'd", "what did")
        ("what've", "what have")
        ("where'd", "where did")
        ("where're", "where are")
        ("where've", "where have")
        ("which're", "which are")
        ("which've", "which have")
        ("who'd've", "who would have")
        ("who're", "who are")
        ("who's", "who has")
        ("who've", "who have")
        ("why'd", "why did")
        ("why're", "why are")
        ("won't", "will not")
        ("would've", "would have")
        ("wouldn't", "would not")
        ("wouldn't've", "would not have")
        ("you'll", "you will")
        ("you're", "you are")
        ("you've", "you have")
        ] |> Map

    /// Tryfind contraction and expand
    let expand (word: string): option<string>=
        if word.Contains("'") then 
            match englishContractions.TryFind word with
            | Some expandedWord -> Some expandedWord
            | None -> None
        else Some word

    let expandContractions (textItem: string) = 
        textItem.Split(" ")
        |> Array.choose expand
        |> String.concat(" ")

module Tokenization =

    /// NGrams Tokenizer 
    let nGrams (n: int) (text: string) = 
        text.Split(" ")
        |> Array.windowed n
        |> Array.map (String.concat(" "))

module NltkData = 

    let stopWords = 
        [
        "i"
        "me"
        "my"
        "myself"
        "we"
        "our"
        "ours"
        "ourselves"
        "you"
        "you're"
        "you've"
        "you'll"
        "you'd"
        "your"
        "yours"
        "yourself"
        "yourselves"
        "he"
        "him"
        "his"
        "himself"
        "she"
        "she's"
        "her"
        "hers"
        "herself"
        "it"
        "it's"
        "its"
        "itself"
        "they"
        "them"
        "their"
        "theirs"
        "themselves"
        "what"
        "which"
        "who"
        "whom"
        "this"
        "that"
        "that'll"
        "these"
        "those"
        "am"
        "is"
        "are"
        "was"
        "were"
        "be"
        "been"
        "being"
        "have"
        "has"
        "had"
        "having"
        "do"
        "does"
        "did"
        "doing"
        "a"
        "an"
        "the"
        "and"
        "but"
        "if"
        "or"
        "because"
        "as"
        "until"
        "while"
        "of"
        "at"
        "by"
        "for"
        "with"
        "about"
        "against"
        "between"
        "into"
        "through"
        "during"
        "before"
        "after"
        "above"
        "below"
        "to"
        "from"
        "up"
        "down"
        "in"
        "out"
        "on"
        "off"
        "over"
        "under"
        "again"
        "further"
        "then"
        "once"
        "here"
        "there"
        "when"
        "where"
        "why"
        "how"
        "all"
        "any"
        "both"
        "each"
        "few"
        "more"
        "most"
        "other"
        "some"
        "such"
        "no"
        "nor"
        "not"
        "only"
        "own"
        "same"
        "so"
        "than"
        "too"
        "very"
        "can"
        "will"
        "just"
        "don"
        "don't"
        "should"
        "should've"
        "now"
        "ain"
        "aren"
        "aren't"
        "couldn"
        "couldn't"
        "didn"
        "didn't"
        "doesn"
        "doesn't"
        "hadn"
        "hadn't"
        "hasn"
        "hasn't"
        "haven"
        "haven't"
        "isn"
        "isn't"
        "ma"
        "mightn"
        "mightn't"
        "mustn"
        "mustn't"
        "needn"
        "needn't"
        "shan"
        "shan't"
        "shouldn"
        "shouldn't"
        "wasn"
        "wasn't"
        "weren"
        "weren't"
        "won"
        "won't"
        "wouldn"
        "wouldn't"
        ]

    let removeStopWords (textItem: string) = 

        let initWords = textItem.Split(" ")
       
        initWords
        |> Array.choose (fun item ->
            if (List.contains item stopWords) then None
            else Some item)
        |> fun xs -> 
            if xs = initWords then Some textItem
            else None

module TermFrequencies = 

    type DocTransfromer = string -> string []

    /// Term-frequencies
    type TermFreq = {Term: string; Tf: float } 

    let tf (docTransformer: DocTransfromer) (doc: string): TermFreq [] =
        let terms = docTransformer doc
        let nTerms = float terms.Length

        terms
        |> Seq.countBy id
        |> Seq.map (fun (term, count) ->
            let tf = (float count)/ nTerms
            {Term = term; Tf = tf})
        |> Seq.toArray
    
    /// Inverse-document frequencies
    type InverseDocFreq = {Term: string; Idf : float } 

    let idf (docTransfromer: DocTransfromer) (docs : string []): Map<string, InverseDocFreq> = 
        let n = docs.Length
        
        let numberOfDocsByTerm = 
            docs
            |> Seq.map docTransfromer
            |> Seq.collect Seq.distinct
            |> Seq.countBy id
          
        numberOfDocsByTerm
        |> Seq.map (fun (term, docsWithTerm) -> 
            let idf = (log (float n / float docsWithTerm))
            term, {Term = term; Idf = idf})
        |> Map
    
    /// Term-frequency inverse-document frequency
    type TermFreqInverseDocFreq = {Term: string; TfIdf: float}

    let tfIdf (doc: string)
              (docTransformer : DocTransfromer)
              (inverseDocFreq: Map<string, InverseDocFreq>): TermFreqInverseDocFreq [] = 
       
       tf docTransformer doc
       |> Array.choose (fun termTf -> 
       match inverseDocFreq.TryFind termTf.Term with
       | None -> None
       | Some termIdf -> 
            let tfIdf = termTf.Tf * termIdf.Idf 
            Some { Term=termTf.Term; TfIdf = tfIdf})