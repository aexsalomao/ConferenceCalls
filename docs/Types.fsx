/// TranscriptParsing.fsx

type TranscriptId = 
    { Ticker: string
      Exchange: string 
      Date: System.DateTime}

type Transcript = 
    { TranscriptId : TranscriptId
      Paragraphs: string [] }


/// Preprocessing
type DocTransfromer = string -> string []