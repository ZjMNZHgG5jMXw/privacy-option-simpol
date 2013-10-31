module Language.SimPOL
  ( module Language.POL
  , module Data.SimPOL.PersonalData
  , module Data.SimPOL.Purpose
  , module Data.SimPOL.Observable
  , module Data.SimPOL.Time
  , module Semantics.SimPOL.Management
  , Contract
  , Obs
  , Universe
  , inspect
  , simulate
  ) where
import Language.POL hiding
  ( Contract
  , PersonalData, attribute, value
  , Purpose, purpose
  , Label
  , advance )
import qualified Language.POL as POL
import Data.SimPOL.PersonalData
import Data.SimPOL.Purpose
import Data.SimPOL.Observable
import Data.SimPOL.Time
import Semantics.SimPOL.Management
import Text.PrettyXHTML ()
import Text.XHtml as XH hiding ( label, value )
type Universe = Management Time
type Obs = Observable Universe
type Contract = POL.Contract PersonalData Purpose Label Universe
inspect :: Contract -> IO ()
inspect c = writeFile "polcontract.html" $ showHtml
  $ h1 << "POL contract inspection"
  XH.+++ h2 << "Human-readable contract"
  XH.+++ (pretty c :: Html)
  XH.+++ h2 << "Machine-readable contract"
  XH.+++ thediv ! [thestyle "font-family:monospace"] << show c
simulate :: Contract -> IO Contract
simulate  = evolve . loop where
  loop :: Contract -> Universe Contract
  loop c = do
    d <- execute c
    if d == Zero
      then return Zero
      else advance >> loop d
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
