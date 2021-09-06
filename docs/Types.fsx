#r "nuget: FSharp.Data"

#load "Common.fsx"

open System
open System.IO
open FSharp.Data

/// TranscriptParsing
type CallId =
    { Ticker: string 
      Exchange: string
      Date: System.DateTime
      FiscalQuarter : int }

type EarningsCall = 
    { CallId : CallId
      Transcript: string [] }

/// EarningsAnnouncementReturn
type Sentiment = 
    | Positive
    | Negative
    | Neutral

type EarningsAnnouncementReturn =
    { EarningsCall: EarningsCall
      TiingoObs: Common.Tiingo.TiingoObs []
      Sentiment: Sentiment option 
      Ear: float option }