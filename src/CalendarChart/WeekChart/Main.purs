module CalendarChart.WeekChart.Main where

import CalendarChart.WeekChart.Chart

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
import Control.Monad.Aff(launchAff,Aff())
import Network.HTTP.Affjax hiding (get)
import qualified Network.HTTP.Affjax as AJ
import Control.Monad.Eff.Console

import qualified Browser.WebStorage as WS

import Data.Either
import Data.Foreign(readString,unsafeFromForeign)
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

import Control.Monad.Free
import Data.Coyoneda

import Control.Monad.Rec.Class (MonadRec)

import Network.RemoteCallback

fetchCont :: (Array Activity -> Eff _ (Unit)) -> Aff _ Unit
fetchCont chartf = do
  strava <- AJ.get "data/activities.json"
  let vals = getStravaFromText $ strava.response
  ra <- AJ.get "data/log.txt"
  pp <- liftEff $ getRAfromText ra.response
  let acts = filter (\(Activity a) -> a.type == Run) $ vals ++ pp
  liftEff $ do
    chartf acts
    callPhantom false

main :: Eff _ Unit
main = launchAff do
  jsd <- externalCall "ChartWeek"
  let dt = (Data.Maybe.Unsafe.fromJust $ fromJSDate $ unsafeFromForeign jsd) :: Date
  fetchCont $ chartWeek dt
