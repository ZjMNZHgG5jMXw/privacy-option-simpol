module Language.SimPOL.Template.Obs where
import Language.SimPOL
import qualified Data.POL.Observable as Obs
import Control.Monad.Trans.Class
at :: Time -> Obs Bool
at t = labeled (nullary "time") (lift now) Obs.== constant t
before :: Time -> Obs Bool
before t = labeled (nullary "time") (lift now) Obs.< constant t
after :: Time -> Obs Bool
after t = labeled (nullary "time") (lift now) Obs.> constant t
between :: Time -> Time -> Obs Bool
between s t = (at s Obs.|| at t) Obs.|| strictbetween s t
strictbetween :: Time -> Time -> Obs Bool
strictbetween s t = after s Obs.&& before t
occurs :: String -> Obs Bool
occurs e = labeled (nullary (e ++ " occurs"))
  (fmap ((elem e) . events) (lift now))
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
