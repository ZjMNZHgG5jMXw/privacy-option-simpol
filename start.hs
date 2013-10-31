import Control.Monad ( join )

import Language.SimPOL as C
import Language.SimPOL.Template
import qualified Data.POL.Observable as Obs

d = join (join disclose)
c1 = d "a"
c2 = d "b"
c3 = d "c"
c4 = d "d"
c5 = d "e"

o1 = at 1
o2 = at 2
o3 = at 3
o4 = at 4
o5 = at 5

-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
