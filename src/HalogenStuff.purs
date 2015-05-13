module HalogenStuff where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A


appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

-- | The state of the application
data State = State [Input]

-- | Inputs to the state machine
data Input = Tick

ui :: forall m eff. (Applicative m) => Component m Input Input
ui = render <$> stateful (State []) update
  where
  render :: State -> H.HTML (m Input)
  render (State s) = H.button
    [ A.onClick $ A.input_ Tick ]
    (H.text <<< (\_ -> "x") <$> s)

  update :: State -> Input -> State
  update (State s) i = State (i:s)

main = do
  Tuple node _ <- runUI ui
  appendToBody node
