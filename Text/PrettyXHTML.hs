module Text.PrettyXHTML where
import Semantics.POL.HumanReadable as HR
  ( PrettyDriver
  , text, mark, emph, items, indent, (+++)
  )
import qualified Text.XHtml as XH
  ( Html
  , (+++), stringToHtml, ordList, thespan, paragraph
  , (!), thestyle
  )
instance PrettyDriver XH.Html where
  text    = XH.stringToHtml
  emph    = (XH.thespan XH.! [XH.thestyle "color:Red"]) . XH.stringToHtml
  mark    = (XH.thespan XH.! [XH.thestyle "color:DarkBlue"]) . XH.stringToHtml
  items   = XH.ordList
  indent  = (. XH.paragraph) . (XH.+++)
  (+++)   = (XH.+++)
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
