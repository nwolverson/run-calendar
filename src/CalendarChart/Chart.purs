module CalendarChart.Chart where

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
import CalendarChart.ChartUtil

import Data.Foldable(sum,find)


succWrap :: forall a. (Enum a) => a -> a
succWrap x = case succ x of
  Just a -> a
  Nothing -> bottom

-- date as it used to be...
date' :: Int -> Month -> Int -> Maybe Date
date' yearN month dayN =
  date year month day
  where
    year = Year yearN
    day = DayOfMonth dayN

lastDay :: Int -> Month -> Maybe Date
lastDay year month = case succ month of
  Nothing -> date' (year+1) January 0
  Just m -> date' year m 0 -- 0th day is end of last month

monthPath :: Int -> Month -> Number -> Maybe String
monthPath year month cellSize = do
  t0 <- date' year month 1
  t1 <- lastDay year month
  let day = dayNumber <<< dayOfWeek
  let w0 = week t0
  let w1 = week t1
  return $ "M" ++ show ((w0 + 1.0) * cellSize) ++ "," ++ show ((day t0) * cellSize)
        ++ "H" ++ show (w0 * cellSize) ++ "V" ++ show (7.0 * cellSize)
        ++ "H" ++ show (w1 * cellSize) ++ "V" ++ show (((day t1)+1.0)* cellSize)
        ++ "H" ++ show ((w1 + 1.0) * cellSize) ++ "V" ++ (show 0.0)
        ++ "H" ++ show ((w0 + 1.0) * cellSize) ++ "Z"

yearRange year = Arr.mapMaybe dayOf yearRange'
  where
    start = date' year January 1
    yearRange' = fromMaybe [] $ Arr.mapMaybe <$> (addDays <$> start) <*> Just (Arr.range 0 364)

monthTotal :: Map Date Number -> Date -> Number
monthTotal input m =
  let sameMonth d1 d2 = (year d1) == (year d2) && (month d1) == (month d2)
      days = filter (sameMonth m) $ keys input
      values = flip dayval input <$> days
  in
    sum values

monday :: Date -> Maybe Date
monday d =
  find isMonday days
  where
    days = Arr.mapMaybe (addDays d) (Arr.range 0 6)
    isMonday :: Date -> Boolean
    isMonday d = dayOfWeek d == Monday

monthChart :: forall a. (Appendable a) => Map Date Number -> (a Int) -> D3Eff (Selection Int)
monthChart input yearSelect =
  let cellSize = 17.0
      width = 960.0
      height = cellSize * 7.0
      margin = {top: 20.0, right: 20.0, bottom: 2.0, left: 20.0}
      colorClasses = (\i -> "q" ++ show i) <$> Arr.range 9 0
      monthTotalHeight = 0.0
  in do
    color <- thresholdScale
      .. domain $ toNumber <$> [5, 10, 15, 20, 25, 30, 40, 50, 70]
      .. range colorClasses
      .. toFunction

    svg <- yearSelect ... mkSvg width height margin

    rect <- svg ... selectAll' "rect.day"
      .. bind' (\y -> yearRange y)
    update <- rect ... enter .. append "rect"
      .. attr "class" "day"
      .. attr "width" cellSize
      .. attr "height" cellSize
      .. attr' "y" (\d -> dayNumber (dayOfWeek d) * cellSize)
      .. attr' "x" (\d -> (week d) * cellSize)

    update ... append "title"
      .. text' (\d -> fullDateFormat (toJSDate d))

    svg ... append "text"
      .. attr "class" "yearTitle"
      .. attr "transform" ("translate(-6," ++ show (cellSize * 3.5) ++ ")rotate(-90)")
      .. style "text-anchor" "middle"
      .. text' (\d -> show d)

    svg ... selectAll' ".monthTotal"
        .. bind' (\y -> Arr.mapMaybe (\m -> date' y m 1) $ enumFromTo January December)
      .. enter .. append "text"
        .. attr "class" "monthTotal"
        .. attr' "x" (\d -> show ((fromMaybe 0.0 $ week <$> monday d) * cellSize))
        .. attr "y" (-5.0) -- um.
        .. text' (\d -> (formatDate "%b" $ toJSDate d) ++ ": " ++ (format ".0f" $ monthTotal input d))

    update ... selectionFilter' (\d -> dayval d input > 0.0)
        .. attr' "class" (\d -> "day " ++ color (dayval d input))
      .. select "title"
        .. text' (\d -> fullDateFormat (toJSDate d) ++ ": " ++ (format ".1f" $ dayval d input))

    svg ... append "g"
        .. attr "transform" ("translate(0," ++ show monthTotalHeight ++ ")")
      .. selectAll' ".month"
        .. bind' (\y -> Tuple y <$> enumFromTo January December)
      .. enter .. append "path"
         .. attr "class" "month"
         .. attr' "d" (\(Tuple y m) -> fromMaybe "" $ monthPath y m cellSize)

    return svg

monthCharts input year count = do
  rootSelect "div.monthchart" .. selectAll "svg" .. remove

  sel <- rootSelect "div.monthchart"
    .. selectAll "svg"
      .. bind_ (Arr.range year $ year+count-1)
  sel ... enter .. monthChart input


chartMonths :: Int -> Array Activity -> D3Eff (Unit)
chartMonths y x = void $ monthCharts (buildMap x) (2015-y+1) y
