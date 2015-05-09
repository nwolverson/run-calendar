module CalendarChart.Chart where

import Graphics.D3.Base
import Graphics.D3.Util
import Graphics.D3.Selection
import Graphics.D3.Scale
import Graphics.D3.Request
import Graphics.D3.SVG.Axis

import Data.Date
import Data.Enum
import Data.Map
import Data.Maybe
import Data.Tuple
import qualified Data.Array as Arr

import CalendarChart.Util
import CalendarChart.Activities

import Data.Foldable(sum,find)

type Margin = { top :: Number, right :: Number, bottom :: Number, left :: Number }

mkSvg :: forall a s. (Appendable s) => Number -> Number -> Margin -> (s a) -> D3Eff (Selection a)
mkSvg width height margin sel =
  sel ... append "svg"
    .. attr "width" (width + margin.left + margin.right)
    .. attr "height" (height + margin.top + margin.bottom)
  .. append "g"
    .. attr "transform" ("translate(" ++ show margin.left ++ "," ++ show margin.top ++ ")")


dayRange start count = Arr.mapMaybe dayOf range
  where
    range = Arr.mapMaybe (addDays start) $ Arr.range 0 (count-1)

chartDays :: Map Date Number -> Date -> Number -> D3Eff (Selection Date)
chartDays input date count = do
  let val = flip dayval $ input
      margin = {top: 0, right: 0, bottom: 0, left: 0}
      width = 70 - margin.left - margin.right
      height = 30 - margin.top - margin.bottom
      dates = dayRange date count
  ysc <- linearScale
    .. domain [0, max (flip dayval input) dates]
    .. range [0, 30]
    .. toFunction
  svg <- rootSelect "div.weekchart"
    .. mkSvg width height margin
  svg ... selectAll "rect"
      .. bind $ dates
    .. enter ..append "rect"
      .. attr "width" 8
      .. attr' "height" $ (ysc <<< val)
      .. attr'' "x" (\_ i -> i * 10)
      .. attr'' "y" (\a i -> 30 - (ysc $ val a))
      .. style "fill" "#E7E7E7"
      .. style "stroke" "none"
    .. append "title"
      .. text' (\x -> show x ++ ": " ++ (show $ val x))

succWrap :: forall a. (Enum a) => a -> a
succWrap x = case succ x of
  Just a -> a
  Nothing -> firstEnum

lastDay :: Number -> Month -> Maybe Date
lastDay year month = case succ month of
  Nothing -> date (year+1) January 0
  Just m -> date year m 0 -- 0th day is end of last month

monthPath :: Number -> Month -> Number -> Maybe String
monthPath year month cellSize = do
  t0 <- date year month 1
  t1 <- lastDay year month
  let day d = ((fromEnum $ dayOfWeek d) + 6) % 7
  let w0 = week t0
  let w1 = week t1
  return $ "M" ++ show ((w0 + 1) * cellSize) ++ "," ++ show ((day t0) * cellSize)
        ++ "H" ++ show (w0 * cellSize) ++ "V" ++ show (7 * cellSize)
        ++ "H" ++ show (w1 * cellSize) ++ "V" ++ show (((day t1)+1)* cellSize)
        ++ "H" ++ show ((w1 + 1) * cellSize) ++ "V" ++ (show 0)
        ++ "H" ++ show ((w0 + 1) * cellSize) ++ "Z"

yearRange year = Arr.mapMaybe dayOf yearRange'
  where
    start = date year January 01
    yearRange' = fromMaybe [] $ Arr.mapMaybe <$> (addDays <$> start) <*> Just (Arr.range 0 364)

monthTotal :: Map Date Number -> Date -> Number
monthTotal input m =
  let sameMonth d1 d2 = (year d1) == (year d2) && (month d1) == (month d2)
      days = Arr.filter (sameMonth m) $ keys input
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

bind' :: forall oldData newData. (oldData -> [newData]) -> Selection oldData -> D3Eff (Update newData)
bind' = ffi ["fn", "selection", ""] "selection.data(fn)"

selectAll' :: forall d. String -> Selection d -> D3Eff (Selection d)
selectAll' = ffi ["selector", "selection", ""] "selection.selectAll(selector)"



monthChart :: forall a. (Appendable a) => Map Date Number -> (a Number) -> D3Eff (Selection Number)
monthChart input yearSelect =
  let cellSize = 17
      width = 960
      height = cellSize * 7
      margin = {top: 20, right: 20, bottom: 2, left: 20}
      colorClasses = (\i -> "q" ++ show i) <$> Arr.range 9 0
      monthTotalHeight = 0
  in do
    color <- thresholdScale
      .. domain [5, 10, 15, 20, 25, 30, 40, 50, 70]
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
      .. text' show

    svg ... append "text"
      .. attr "class" "yearTitle"
      .. attr "transform" ("translate(-6," ++ show (cellSize * 3.5) ++ ")rotate(-90)")
      .. style "text-anchor" "middle"
      .. text' (\d -> show d)

    svg ... selectAll' ".monthTotal"
        .. bind' (\y -> Arr.mapMaybe (\m -> date y m 1) $ enumFromTo January December)
      .. enter .. append "text"
        .. attr "class" "monthTotal"
        .. attr' "x" (\d -> show ((fromMaybe 0 $ week <$> monday d) * cellSize))
        .. attr "y" (-5) -- um.
        .. text' (\d -> (formatDate "%b" $ toJSDate d) ++ ": " ++ (format ".0f" $ monthTotal input d))

    update ... selectionFilter' (\d -> dayval d input > 0)
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
      .. bind (Arr.range year $ year+count-1)
  sel ... enter .. monthChart input
