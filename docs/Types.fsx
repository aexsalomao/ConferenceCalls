#r "nuget: FSharp.Data"

#load "Common.fsx"

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

/// Multinomial Naive Bayes Classifier
type Token = string
type Tokenizer = string -> Token []

type Document = Token []
type Class = Sentiment
type LabelledDocument = Document * Class

type Count = int
type TokenCount = Token * Count
type BagOfWords = TokenCount []
type LabelledBagOfWords = BagOfWords * Class

type NbClassifier = BagOfWords -> Class

type Prior = float
type Likelihood = float
type TokenLikelihoods = Map<Token, Likelihood>

type TokenScore = float
type DocumentScore = float

type Accuracy = float