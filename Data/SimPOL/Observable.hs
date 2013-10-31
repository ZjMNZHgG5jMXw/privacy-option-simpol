module Data.SimPOL.Observable where
import Data.POL.Observable ( ObservableT )
import qualified Data.POL.Observable as POL
import Text.Printf
data Label
  = NoLabel
  | Nullary String
  | Unary String Label
  | Binary String Label Label
  deriving (Eq, Ord)
instance POL.Label Label where
  nolabel = NoLabel
  nullary = Nullary
  unary   = Unary
  binary  = Binary
type Observable = ObservableT Label
instance Show Label where
  show NoLabel        = "NoLabel"
  show (Nullary s)    = s
  show (Unary s l)    = printf "%s (%s)" s (show l)
  show (Binary s k l) = printf "(%s) %s (%s)" (show k) s (show l)
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
