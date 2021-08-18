/// TranscriptParsing.fsx

type TranscriptId = 
    | Indexed of ticker:string * exchange:string * date: System.DateTime

type Transcript = 
    { TranscriptId : TranscriptId
      Paragraphs: string [] }