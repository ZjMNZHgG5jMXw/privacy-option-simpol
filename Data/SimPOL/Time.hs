module Data.SimPOL.Time where
import Data.POL.Time
data Timeline = Timeline
  { index   :: Int
  , events  :: [String]
  , next    :: Maybe Timeline
  }
instance Zero Timeline where
  zero = Timeline { index   = 0
                  , events  = []
                  , next    = Nothing
                  }
instance Discrete Timeline where
  advance t         = maybe infinity id (next t)
    where infinity  = zero { index = succ (index t) }
type Time = Timeline
instance Show Timeline where
  show = show . index
instance Eq Timeline where
  s == t = index s == index t
instance Ord Timeline where
  compare s t = compare (index s) (index t)
instance Num Timeline where
  s + t         = Timeline  { index = index s + index t
                            , events = []
                            , next = Nothing }
  s - t         = Timeline  { index = index s - index t
                            , events = []
                            , next = Nothing }
  s * t         = Timeline  { index = index s * index t
                            , events = []
                            , next = Nothing }
  abs t         = Timeline  { index = abs (index t)
                            , events = []
                            , next = Nothing }
  signum t      = Timeline { index = signum (index t)
                            , events = []
                            , next = Nothing }
  fromInteger i = Timeline  { index = fromInteger i
                            , events = []
                            , next = Nothing }
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
