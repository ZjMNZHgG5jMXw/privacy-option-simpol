module Language.SimPOL.Template.Disclose where
import Language.SimPOL
disclose :: String -> String -> String -> Contract
disclose a v p = pdata
  ( PersonalData  { attribute = a, value = v } )
  ( Purpose       { purpose = p } )
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
