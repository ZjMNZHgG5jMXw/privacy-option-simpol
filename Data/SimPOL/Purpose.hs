module Data.SimPOL.Purpose where
import Semantics.POL.HumanReadable
newtype Purpose = Purpose { purpose :: String }
  deriving (Eq, Ord)
instance Show Purpose where
  show = purpose
instance PrettyPrintable Purpose where
  pretty = text . purpose
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
