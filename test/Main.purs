module Test.Main where

import Prelude
import Test.Unit


import Data.Date
import CalendarChart.Activities
import CalendarChart.Main

testMain = runTest do
  test "week starts with monday" do
    assert "monday not day 0" $ dayNumber Monday == 0.0
  test "mergeActs" do
    let ra = Activities RunningAhead []
    let sa = Activities StravaFile []
    assert "2 ra collapse" $ mergeActs ra [ra] == [ra]
    assert "ra neq strava" $ mergeActs ra [sa] == [sa, ra]
