module Main where

import Prelude hiding (div)

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Halogen (ClassName(ClassName), Component, ComponentDSL, ComponentHTML, component, gets, modify)
import Halogen.Aff (awaitBody, HalogenEffects, runHalogenAff)
import Halogen.HTML (button, div, HTML, h1_, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_)
import Halogen.VDom.Driver (runUI)
import Data.Maybe (Maybe(..))
import Modal (modal)
import Models (Query(..), State)

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI view unit body

view :: forall eff. Component HTML Query Unit Void (Aff eff)
view =
  component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

    initialState :: State
    initialState =
      { message: "PureScript Modal"
      , isModalOneVisible: false
      , isModalTwoVisible: false
      }

    render :: State -> ComponentHTML Query
    render state =
      div [ class_ <<< ClassName $ "flex flex-column helvetica items-center justify-center tc" ]
        [ h1_ [ text state.message ]
        , modal state.isModalOneVisible state someArbitraryViewFunctionOne ToggleModalOne
        , modal state.isModalTwoVisible state someArbitraryViewFunctionTwo ToggleModalTwo
        , button [ class_ <<< ClassName $ buttonClasses <> " " <> "ba b--blue bg-blue hover-bg-dark-blue mt1", onClick <<< input_ $ ToggleModalOne ] [ text "Launch Modal 1" ]
        , button [ class_ <<< ClassName $ buttonClasses <> " " <> "ba b--green bg-green hover-bg-dark-green mt1", onClick <<< input_ $ ToggleModalTwo ] [ text "Launch Modal 2" ]
        ]

    eval :: Query ~> ComponentDSL State Query Void (Aff eff)
    eval (ToggleModalOne next) = do
      isModalOneVisible <- gets _.isModalOneVisible
      modify (_ { isModalOneVisible = not isModalOneVisible })
      pure next
    eval (ToggleModalTwo next) = do
      isModalTwoVisible <- gets _.isModalTwoVisible
      modify (_ { isModalTwoVisible = not isModalTwoVisible })
      pure next

buttonClasses :: String
buttonClasses = "ba bw0 f5 outline-0 pa3 pointer white"

someArbitraryViewFunctionOne :: State -> ComponentHTML Query
someArbitraryViewFunctionOne state =
  div [ class_ <<< ClassName $ "flex flex-column items-center justify-center pa6" ]
    [ h1_ [ text "This is the First Modal" ]
    , button [ class_ <<< ClassName $ buttonClasses <> " " <> "ba b--red bg-red hover-bg-dark-red mt1", onClick <<< input_ $ ToggleModalOne ] [ text "Close" ]
    ]


someArbitraryViewFunctionTwo :: State -> ComponentHTML Query
someArbitraryViewFunctionTwo state =
  div [ class_ <<< ClassName $ "flex flex-column items-center justify-center pa6" ]
    [ h1_ [ text "This is Modal Two" ]
    , button [ class_ <<< ClassName $ buttonClasses <> " " <> "ba b--red bg-red hover-bg-dark-red mt1", onClick <<< input_ $ ToggleModalTwo ] [ text "Close" ]
    ]
