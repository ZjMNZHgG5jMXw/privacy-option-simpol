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
import Text.XHtml.Strict ( HTML ( .. ) )
newtype HtmlContract = HtmlContract { html :: XH.Html }
instance HTML HtmlContract where
  toHtml = html
  toHtmlFromList = toHtmlFromList . map html
instance PrettyDriver HtmlContract where
  text    = HtmlContract . XH.stringToHtml
  emph    = HtmlContract . (XH.thespan XH.! [XH.thestyle "color:Red"]) . XH.stringToHtml
  mark    = HtmlContract . (XH.thespan XH.! [XH.thestyle "color:DarkBlue"]) . XH.stringToHtml
  items   = HtmlContract . XH.ordList . map html
  indent  = (HtmlContract .) . (. XH.paragraph . html) . (XH.+++) . html
  (+++)   = (HtmlContract .) . (. html) . (XH.+++) . html -- HtmlContract . (. html) . ((XH.+++) . html)
-- vim: ft=haskell:sts=2:sw=2:et:nu:ai
