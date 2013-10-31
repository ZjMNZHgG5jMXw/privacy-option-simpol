module Language.SimPOL.Template.Option where
import Prelude hiding ( or )
import Language.SimPOL
option :: Contract -> Contract
option = or zero
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
