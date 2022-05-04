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

/// Tokenizination
type Token = string
type Tokenizer = string -> Token []
type Document = Token []
type Class = 
    | Positive
    | Negative
    | Neutral
type LabelledDocument = Document * Class

/// Bag of Words 
type Count = int
type BagOfWords = (Token * Count) []
type LabelledBagOfWords = BagOfWords * Class

/// Naive Bayes Classifier
type NbClassifier = BagOfWords -> Class
type Prior = float
type Likelihood = float
type TokenLikelihoods = Map<Token, Likelihood>
type TokenScore = float
type DocumentScore = float

type Accuracy = float

