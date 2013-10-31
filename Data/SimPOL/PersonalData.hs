module Data.SimPOL.PersonalData where
import Semantics.POL.HumanReadable
import Text.Printf
data PersonalData = PersonalData
  { attribute :: String
  , value     :: String
  } deriving (Eq, Ord)
instance Show PersonalData where
  show a = printf "%s: %s" (attribute a) (value a)
instance PrettyPrintable PersonalData where
  pretty = text . show
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
