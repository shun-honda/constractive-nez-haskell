module Type where

data Type =
  TNumber -- Bs Types
  | TInteger
  | TFloat
  | TBoolean
  | TString
  | TName
  | TText -- end Bs Types
  | TObject
  | TTagObject
  | TKeyValueBase
  | TKeyValue
  | TIterationDelim -- Bd types
  | TKeyValueDelim
  | TClose
  | TOpen
  | TTOpen
  | TTClose
  | TTEnd
  | TWS -- end Bd Types
  | TOpenTag
  | TCloseTag
  | TSequence [Type]
  deriving (Show,Eq,Ord)
