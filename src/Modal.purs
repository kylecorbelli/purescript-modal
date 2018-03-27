module Modal where

import Prelude hiding (div)

import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen (ClassName(..), Component, ComponentDSL, ComponentHTML, component, gets, modify)
import Halogen.HTML (button, div, h1_, HTML, text)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_)

type State =
  { message :: String
  , isModalVisible :: Boolean
  }

data Query a
  = ToggleModal a

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
    initialState =
      { message: "PureScript Modal"
      , isModalVisible: false
      }

    render :: State -> ComponentHTML Query
    render state =
      div [ class_ <<< ClassName $ "flex flex-column helvetica items-center justify-center tc" ]
        [ h1_ [ text state.message ]
        , theModal state.isModalVisible state someArbitraryViewFunction
        , button [ class_ <<< ClassName $ buttonClasses, onClick <<< input_ $ ToggleModal ] [ text "Launch Modal" ]
        ]

    eval :: Query ~> ComponentDSL State Query Void (Aff eff)
    eval (ToggleModal next) = do
      isModalVisible <- gets _.isModalVisible
      modify (_ { isModalVisible = not isModalVisible })
      pure next

theModal :: Boolean -> State -> (State -> HTML Void (Query Unit)) -> HTML Void (Query Unit)
theModal isVisible state viewFunction =
  let
    displayClass = if isVisible then "flex" else "dn"
    containerClasses = "bottom-0 fixed items-center justify-center left-0 right-0 top-0" <> " " <> displayClass
  in
    div [ class_ <<< ClassName $ containerClasses ]
      [ div [ class_ <<< ClassName $ "bg-white flex items-center justify-center mw7 relative w-90 z-4" ] [ viewFunction state ]
      , div [ class_ <<< ClassName $ "bg-black bottom-0 fixed left-0 o-80 right-0 top-0 z-1", onClick <<< input_ $ ToggleModal ] []
      ]

buttonClasses :: String
buttonClasses = "ba b--blue bg-blue bw0 f5 hover-bg-dark-blue outline-0 pa3 pointer white"

someArbitraryViewFunction :: State -> ComponentHTML Query
someArbitraryViewFunction state =
  div [ class_ <<< ClassName $ "flex flex-column items-center justify-center pa6" ]
    [ h1_ [ text state.message ]
    , button [ class_ <<< ClassName $ buttonClasses, onClick <<< input_ $ ToggleModal ] [ text "Close" ]
    ]
