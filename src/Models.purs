module Models where

type State =
  { message :: String
  , isModalOneVisible :: Boolean
  , isModalTwoVisible :: Boolean
  }

data Query a
  = ToggleModalOne a
  | ToggleModalTwo a
