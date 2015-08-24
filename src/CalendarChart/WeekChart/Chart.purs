module CalendarChart.WeekChart.Chart where

import CalendarChart.ChartUtil

import Prelude hiding (append)

import Graphics.D3.Base
import Graphics.D3.Util hiding (min,max)
import Graphics.D3.Selection
import Graphics.D3.Scale
import Graphics.D3.Request
import Graphics.D3.SVG.Axis


import Data.Date
import Data.Date.UTC
import Data.Enum
import Data.Map
import Data.Maybe
import Data.Tuple hiding (lookup)
import Data.Int(fromNumber,toNumber)
import qualified Data.Array as Arr
import Math
import Data.List(filter)

import CalendarChart.Util
import CalendarChart.Activities

import Data.Foldable(sum,find)

chartDays :: Map Date Number -> Date -> Int -> D3Eff (Selection Date)
chartDays input date count = do
  let val = flip dayval $ input
      margin = {top: 0.0, right: 0.0, bottom: 0.0, left: 0.0}
      width = 70.0 - margin.left - margin.right
      height = 30.0 - margin.top - margin.bottom
      dates = dayRange date count
  ysc <- linearScale
    .. domain [0.0, max' val dates]
    .. range [0.0, 30.0]
    .. toFunction
  svg <- rootSelect "div.weekchart"
    .. mkSvg width height margin
  svg ... selectAll "rect"
      .. bind_ $ dates
    .. enter ..append "rect"
      .. attr "width" 8.0
      .. attr' "height" $ (ysc <<< val)
      .. attr'' "x" (\_ i -> i * 10.0)
      .. attr'' "y" (\a i -> 30.0 - (ysc $ val a))
      .. style "fill" "#E7E7E7"
      .. style "stroke" "none"
    .. append "title"
      -- .. text' (\x -> show x ++ ": " ++ (show $ val x))
      .. text' (\d -> fullDateFormat (toJSDate d) ++ ": " ++ (format ".1f" $ val d))


chartWeek :: Date -> Array Activity -> D3Eff (Unit)
chartWeek date x = void $ chartDays (buildMap x) date 7


dayRange start count = Arr.mapMaybe dayOf range
  where
    range = Arr.mapMaybe (addDays start) $ Arr.range 0 (count-1)
