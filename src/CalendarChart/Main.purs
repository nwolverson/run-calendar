module CalendarChart.Main where


import Prelude

import Data.Maybe
import Data.JSON
import Data.Const
import Data.Void

import CalendarChart.Chart
import CalendarChart.Activities
import CalendarChart.Strava
import CalendarChart.RA
import CalendarChart.Util

import CalendarChart.Component.Types
import CalendarChart.Component.Main
import CalendarChart.Component.Chart
import CalendarChart.Downloads

import Data.Array(mapMaybe,length,filter,concat,(..),findIndex,modifyAt,(:))
import Data.Date
import Data.Time
import Data.Tuple
import Data.Foldable(for_)

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(launchAff,runAff,Aff())
import Network.HTTP.Affjax hiding (get)
import Control.Monad.Eff.Console

import qualified Browser.WebStorage as WS

import Data.Either
import Data.Foreign(readString)
import Control.Bind
import DOM
import DOM.File.Types
import DOM.Node.Types
import DOM.HTML.Types
import DOM.Node.Document
import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)

import Halogen
import Halogen.Component
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Properties as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Types as ET
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E
import qualified Halogen.Query.StateF as S
import Data.Functor (($>))
import Control.Plus (empty)
import qualified Data.Int as I

import Data.Functor.Coproduct(left)

import Control.Monad.Free
import Data.Coyoneda

import Control.Monad.Rec.Class (MonadRec)

import Network.RemoteCallback

ui :: forall p eff. State -> InstalledComponent State ChartState Input ChartInput (Aff AppEffects) (Const Void) ChartPlaceholder p
ui s = install container \ChartPlaceholder -> Tuple chartUi { elt : Nothing, state : s }

isOld :: Maybe Date -> Date -> Boolean
isOld Nothing _ = true
isOld (Just old) current =
  diff > Hours 1.0
  where
    t1 = toEpochMilliseconds old
    t2 = toEpochMilliseconds current
    diff = toHours $ t2 - t1

mainInteractive :: Aff AppEffects Unit
mainInteractive =  do
  cachedToken <- liftEff $ WS.getItem WS.localStorage stravaTokenKey
  savedState <- liftEff $ WS.getItem WS.localStorage savedStateKey

  { node: node, driver: driver } <- runUI (ui defaultState) $ installedState defaultState
  liftEff $ (Halogen.Util.appendToBody node :: Eff AppEffects Unit)

  let initialState = fromMaybe defaultState $ savedState >>= decode
  driver (left $ action $ SavedState initialState)
  current <- liftEff now
  case {s: initialState, t: cachedToken} of
    { s: State { lastPull: lp }, t: Just token } | isOld lp current -> do
      recentActs <- downloadedStrava token
      return unit
      driver (left $ action $ StravaDownloadData recentActs current)
    _ -> return unit

  return unit

--ugh launchAff gives duplicate exception effect
main :: Eff AppEffects Unit
main = runAff (pure (return unit)) (pure (return unit)) mainInteractive
