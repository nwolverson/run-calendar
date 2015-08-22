module CalendarChart.Component.Types where

import Prelude
import Halogen
import Halogen.Component
import Data.JSON
import DOM.HTML.Types(HTMLElement())
import Data.Maybe
import Data.Date
import CalendarChart.Activities

type AppEffects = HalogenEffects (d3 :: Graphics.D3.Base.D3, feff :: CalendarChart.Util.FileEffect, console :: Control.Monad.Eff.Console.CONSOLE)

data ChartPlaceholder = ChartPlaceholder

instance eqChartPlaceholder :: Eq ChartPlaceholder where
  eq _ _ = true

instance ordChartPlaceholder :: Ord ChartPlaceholder where
  compare _ _ = EQ


type ChartState = { elt :: Maybe HTMLElement, state :: State  }
data ChartInput a = ChartInput State a | Init HTMLElement a


data Input a =
  DownloadStrava a
  | RaFileInput a
  | StravaFileInput a
  | StravaFileInputControl HTMLElement a
  | RaFileInputControl HTMLElement a

  | Years Int a
  -- Driver input:
  | StravaDownloadData (Array Activity) Date a
  | SavedState State a

data State = State { data:: Array Activities, years:: Int, lastPull:: Maybe Date }

defaultState :: State
defaultState = State { data: [], years: 1, lastPull: Nothing }

instance stateToJSON :: ToJSON State where
  toJSON (State { data = d, years = y, lastPull = lastPull }) =
    object [ "data" .= d, "years" .= y, "lastPull" .= lastPull ]

instance stateFromJSON :: FromJSON State where
  parseJSON (JObject o) = do
    d <- o .: "data"
    y <- o .: "years"
    lp <- o .: "lastPull"
    return $ State { data: d, years: y, lastPull : lp }
  parseJSON _ = fail "Not object"
