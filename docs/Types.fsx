#r "nuget: FSharp.Data"

#load "Common.fsx"

open System
open System.IO
open FSharp.Data
open Common
open Common.Tiingo

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
      TiingoObs: TiingoObs [] 
      Sentiment : Sentiment option} with
      // Returns Window
      member this.ReturnWindow = 
        1.
     // Earnigns Announcement Return
      member this.Ear = 
        this.ReturnWindow