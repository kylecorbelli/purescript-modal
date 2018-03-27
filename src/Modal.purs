module Modal where

import Prelude hiding (div)

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen (Component, ComponentDSL, ComponentHTML, component)
import Halogen.HTML (HTML, div, h1, text)

type State = Int

data Query a
  = NoOp a

modal :: forall eff. Component HTML Query Unit Void (Aff eff)
modal =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState = 3

    render :: State -> ComponentHTML Query
    render state =
      div []
        [ h1 [] [ text "PureScript Modal" ]
        ]

    eval :: Query ~> ComponentDSL State Query Void (Aff eff)
    eval (NoOp next) = do
      pure next
