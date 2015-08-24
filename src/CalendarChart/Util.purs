module CalendarChart.Util where

import Prelude hiding (append)

import Graphics.D3.Base
import Graphics.D3.Selection
import Graphics.D3.Scale

import Data.Maybe
import Data.Date

import Data.Foreign.EasyFFI
import Data.Foreign(Foreign(..),unsafeFromForeign,F(..),readArray)
import Control.Monad.Eff
import DOM
import DOM.File.Types
import DOM.HTML.Types

import Data.Either

--import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(makeAff,Aff())

ffi = unsafeForeignFunction

formatDate :: String -> JSDate -> String
formatDate = unsafeForeignFunction ["fmt"] "d3.time.format(fmt)"

dateToISOString :: JSDate -> String
dateToISOString = ffi ["d"] "d.toISOString()"

format :: String -> Number -> String
format = ffi ["fmt"] "d3.format(fmt)"

parseInt = Global.readInt 10

selectionFilter :: forall d. String -> Selection d -> D3Eff (Selection d)
selectionFilter = unsafeForeignFunction ["selector", "selection", ""] "selection.filter(selector)"

selectionFilter' :: forall d. (d -> Boolean) -> Selection d -> D3Eff (Selection d)
selectionFilter' = unsafeForeignFunction ["fn", "selection", ""] "selection.filter(fn)"

bind' :: forall oldData newData. (oldData -> Array newData) -> Selection oldData -> D3Eff (Update newData)
bind' = ffi ["fn", "selection", ""] "selection.data(fn)"

selectAll' :: forall d. String -> Selection d -> D3Eff (Selection d)
selectAll' = ffi ["selector", "selection", ""] "selection.selectAll(selector)"


parseTsv :: String -> Array Foreign
parseTsv = ffi ["str"] "d3.tsv.parse(str)"


unsafeOnEvent :: forall eff a i r. String -> (i -> Eff eff r) -> (Selection a) -> D3Eff (Selection a)
unsafeOnEvent = ffi ["eventName", "callback", "selection", ""] "selection.on(eventName, function(data) { return callback(data)(); })"
--"
onChange :: forall eff a r. (Foreign -> Eff eff r) -> (Selection a) -> D3Eff (Selection a)
onChange = unsafeOnEvent "change"

onClick' :: forall eff a r. (Foreign -> Eff eff r) -> (Selection a) -> D3Eff (Selection a)
onClick' = unsafeOnEvent "click"

click :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
click = ffi ["elt", ""] "elt.click()"

elementToHtmlElement :: DOM.Node.Types.Element -> Maybe HTMLElement
elementToHtmlElement elt = castElt Just Nothing elt
  where
    castElt :: (HTMLElement -> Maybe HTMLElement) -> (Maybe HTMLElement) -> DOM.Node.Types.Element -> Maybe HTMLElement
    castElt = ffi ["just", "nothing", "elt"] "elt instanceof HTMLElement ? just(elt) : nothing"

nodeFiles :: forall a. Selection a -> Foreign
nodeFiles = ffi ["selection"] "selection.node() !== null ? Array.prototype.slice.call(selection.node().files) : null"
--"
getFile :: forall a. Selection a -> Maybe File
getFile sel = do
  let files = nodeFiles sel
  case Data.Foreign.readArray files of
    Right [ f ] -> Just $ unsafeFromForeign $ f
    _ -> Nothing

elementFiles :: HTMLElement -> Foreign
elementFiles = ffi ["element"] "element !== null ? Array.prototype.slice.call(element.files) : null"

getElementFile :: HTMLElement -> Maybe File
getElementFile sel = do
  let files = elementFiles sel
  case Data.Foreign.readArray files of
    Right [ f ] -> Just $ unsafeFromForeign $ f
    _ -> Nothing

getElementFile' :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) (Maybe File)
getElementFile' e = return $ getElementFile e

fileReader :: Unit -> FileReader
fileReader = ffi [""] "new FileReader()"

fileAsBlob :: File -> Blob
fileAsBlob = ffi ["file"] "file"

foreign import data FileEffect :: !

readAsText :: forall a eff eff2. FileReader -> Blob -> (String -> Eff eff a) -> Eff (feff :: FileEffect | eff2) Unit
readAsText = ffi ["fr", "file", "callback"]
  """function() {
  fr.onloadend = function () {
    callback(fr.result)();
  };
  fr.readAsText(file);
  return {};
}"""

readAsTextAff :: FileReader -> Blob -> Aff _ String
readAsTextAff fr file = makeAff (\error success -> readAsText fr file success)

openWindow :: forall a. String -> String -> String -> Eff (d :: DOM | a) Unit
openWindow = ffi ["url", "windowName", "features", ""] "window.open(url,windowName,features)"

-- oh dear.
callPhantom :: Boolean -> D3Eff Unit
callPhantom = unsafeForeignFunction ["x", ""] "window.callPhantom && window.callPhantom(x);"

-- Sigh, easier than rewriting parsing code
foreignStringify :: Foreign -> String
foreignStringify = ffi ["x"] "JSON.stringify(x)"
