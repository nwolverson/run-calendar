module CalendarChart.Component.Chart where

import CalendarChart.Component.Types

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

import Data.Array(mapMaybe,length,filter,concat,(..),findIndex,modifyAt,(:))
import Data.Date
import Data.Time
import Data.Tuple
import Data.Foldable(for_)

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Class(liftEff)
import Control.Monad.Aff(launchAff,Aff())
import Network.HTTP.Affjax
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

import Control.Monad.Free
import Data.Coyoneda

import Control.Monad.Rec.Class (MonadRec)

import Network.RemoteCallback

import CalendarChart.Downloads
import CalendarChart.Component.Main
import CalendarChart.Component.Chart


savedStateKey = "savedState"

chartUi :: forall p. Component ChartState ChartInput (Aff AppEffects) p
chartUi = component render eval
  where
  render :: Render ChartState ChartInput p
  render _ =  H.div [ initializer, A.classes [H.className "monthchart", H.className "hcl2"] ] [ ]

  -- possibly should use this elt to pass to chart rather than go via css selector?
  initializer = H.Initializer "" (\el -> action (Init el))

  eval :: Eval ChartInput ChartState ChartInput (Aff AppEffects)
  eval (ChartInput (s@ (State { data=acts, years=y })) next) = do
    liftEff' $ log "child chart got state"
    S.modify _ { state = s }

    liftEff' $ WS.setItem WS.localStorage savedStateKey $ encode s
    let onLoad = \_ -> ((chart y $ allActivities acts) :: (Eff AppEffects Unit))
    liftEff' $ DOM.Event.EventTarget.addEventListener DOM.Event.EventTypes.load (DOM.Event.EventTarget.eventListener onLoad) false <<< DOM.HTML.Types.windowToEventTarget =<< window

    liftEff' $ chart y $ allActivities acts
    pure next
  eval (Init el next) = do
    liftEff' $ log "Child chart got init"
    S.modify _{ elt = Just el }
    pure next


chart y = chartMonths y <<< filter (\(Activity a) -> a.type == Run)
