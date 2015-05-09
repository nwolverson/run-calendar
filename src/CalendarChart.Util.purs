module CalendarChart.Util where

import Graphics.D3.Base
import Graphics.D3.Selection
import Graphics.D3.Scale

import Data.Maybe
import Data.Date

import Data.Foreign.EasyFFI
import Data.Foreign(Foreign(..),unsafeFromForeign,F(..),readArray)
import Control.Monad.Eff
import DOM
import DOM.File

import Data.Either
import Debug.Trace

ffi = unsafeForeignFunction

formatDate :: String -> JSDate -> String
formatDate = unsafeForeignFunction ["fmt"] "d3.time.format(fmt)"

format :: String -> Number -> String
format = ffi ["fmt"] "d3.format(fmt)"

parseInt = Global.readInt 10

unsafeDomain = ffi ["domain", "scale", ""] "scale.domain(domain)"
unsafeRange = ffi ["values", "scale", ""] "scale.range(values)"
unsafeCopy = ffi ["scale", ""] "scale.copy()"
unsafeToFunction = ffi ["scale", ""] "scale.copy()"
unsafeInvert = ffi ["scale", ""] "scale.copy().invert"
unsafeRangeRound = ffi ["values", "scale", ""] "scale.rangeRound(values)"
unsafeInterpolate = ffi ["factory", "scale", ""] "scale.interpolate(factory)"
unsafeClamp = ffi ["bool", "scale", ""] "scale.clamp(bool)"
unsafeNice count = case count of
  Nothing -> ffi ["scale", ""] "scale.nice()"
  Just c -> ffi ["count", "scale", ""] "scale.nice(count)" c
unsafeTicks count = case count of
  Nothing -> ffi ["scale", ""] "scale.ticks()"
  Just c -> ffi ["count", "scale", ""] "scale.ticks(count)" c
unsafeTickFormat count format = case format of
  Nothing -> ffi ["count", "scale", ""] "scale.tickFormat(count)" count
  Just f -> ffi ["count", "format", "scale", ""] "scale.tickFormat(count, format)" count f


foreign import data ThresholdScale :: * -> * -> *

foreign import thresholdScale "var thresholdScale = d3.scale.threshold"
  :: forall r. D3Eff (ThresholdScale Number r)

instance scaleThreshold :: Scale ThresholdScale where
  domain = unsafeDomain
  range = unsafeRange
  copy = unsafeCopy
  toFunction = unsafeToFunction

selectionFilter :: forall d. String -> Selection d -> D3Eff (Selection d)
selectionFilter = unsafeForeignFunction ["selector", "selection", ""] "selection.filter(selector)"

selectionFilter' :: forall d. (d -> Boolean) -> Selection d -> D3Eff (Selection d)
selectionFilter' = unsafeForeignFunction ["fn", "selection", ""] "selection.filter(fn)"


parseTsv :: String -> [Foreign]
parseTsv = ffi ["str"] "d3.tsv.parse(str)"


unsafeOnEvent :: forall eff a i r. String -> (i -> Eff eff r) -> (Selection a) -> D3Eff (Selection a)
unsafeOnEvent = ffi ["eventName", "callback", "selection", ""] "selection.on('change', function() { return callback()(); })"

onChange = unsafeOnEvent "change"

nodeFiles :: forall a. Selection a -> Foreign
nodeFiles = ffi ["selection"] "selection.node() !== null ? Array.prototype.slice.call(selection.node().files) : null"

getFile :: forall a. Selection a -> Maybe File
getFile sel = do
  let files = nodeFiles sel
  case Data.Foreign.readArray files of
    Right [ f ] -> Just $ unsafeFromForeign $ f
    _ -> Nothing

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


-- oh dear.
callPhantom :: Boolean -> D3Eff Unit
callPhantom = unsafeForeignFunction ["x", ""] "window.callPhantom && window.callPhantom(x);"
