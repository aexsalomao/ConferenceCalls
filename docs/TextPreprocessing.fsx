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

    /// Simple english contractions
    let expandContractions (text: string): string= 

        let simpleContractions: Map<string, string>= 
            [|("i'll", "i will")
              ("he'll", "he will")
              ("you're", "you are")
              ("aren't", "are not")
              ("isn't", "is not")
              ("don't", "do not")
              ("didn't", "did not")
              ("doesn't", "does not")
              ("can't", "can not")
              ("couldn't", "could not")
              ("shan't", "shall not")|]
            |> Map
        
        let expand (word: string): option<string>=
            if word.Contains("'") then 
                match simpleContractions.TryFind word 
                | Some expandedWord -> Some expandedWord
                | None -> Some word
            else Some word

        text.Split(" ")
        |> Array.choose expand
        |> String.concat(" ")

module Tokenization =

    open System.Text.RegularExpressions

    /// NGrams Tokenizer 
    let nGrams (n: int) (text: string) = 
        text.Split(" ")
        |> Array.windowed n
        |> Array.map (String.concat(" "))

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
            Some { Term = termTf.Term; TfIdf = tfIdf })