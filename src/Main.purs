module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Halogen.Aff (awaitBody, HalogenEffects, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Modal (modal)

main :: forall eff. Eff (HalogenEffects eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI modal unit body
