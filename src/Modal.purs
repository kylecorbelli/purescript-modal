module Modal where

import Prelude hiding (div)

import Halogen (ClassName(ClassName))
import Halogen.HTML (div, HTML)
import Halogen.HTML.Events (input_, onClick)
import Halogen.HTML.Properties (class_)
import Models (Query, State)

modal :: Boolean -> State -> (State -> HTML Void (Query Unit)) -> (Unit -> Query Unit) -> HTML Void (Query Unit)
modal isVisible state viewFunction onDismiss =
  let
    displayClass = if isVisible then "flex" else "dn"
    containerClasses = "bottom-0 fixed items-center justify-center left-0 right-0 top-0" <> " " <> displayClass
  in
    div [ class_ <<< ClassName $ containerClasses ]
      [ div [ class_ <<< ClassName $ "bg-white flex items-center justify-center mw7 relative w-90 z-4" ] [ viewFunction state ]
      , div [ class_ <<< ClassName $ "bg-black bottom-0 fixed left-0 o-80 right-0 top-0 z-1", onClick <<< input_ $ onDismiss ] []
      ]
