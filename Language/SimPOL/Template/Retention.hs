module Language.SimPOL.Template.Retention where
import Prelude hiding ( until )
import Language.SimPOL
import Language.SimPOL.Template.Option
import Language.SimPOL.Template.Obs
retain :: Time -> Contract -> Universe Contract
retain t c = (\present ->
    until (at (present + t)) (option c)
  ) `fmap` now
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
