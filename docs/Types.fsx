#r "nuget: FSharp.Data"

#load "Common.fsx"

open System
open System.IO
open FSharp.Data

/// TranscriptParsing
type CallId =
    {
        Ticker : string 
        Exchange : string
        Date : System.DateTime
        FiscalQuarter : int     
    }

type EarningsCall = 
    {
        CallId : CallId
        Transcript : string [] 
    }

/// EarningsAnnouncementReturn
type Sentiment = 
    | Positive
    | Negative
    | Neutral

type EarningsAnnouncementReturn =
    {
        EarningsCall : EarningsCall
        TiingoObs : Common.Tiingo.TiingoObs []
        Sentiment : Sentiment option 
        Ear : float option 
    }

/// ClassifyingEarningsCalls

type Label = Sentiment

type Token = string
type Count = int
type TokenCount = Token * Count
type BagOfWords = TokenCount list

type Tf = float 
type Idf = float
type TfIdf = float

type Prior = float
type Likelihood = float

type TokenScore = float
type DocScore = float

type TokenLikelihoods = Map<Token, Likelihood>

type Accuracy = float