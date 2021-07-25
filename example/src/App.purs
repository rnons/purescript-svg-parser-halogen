module App where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Icons (iconCheck, iconCode, iconEye, iconGithub, iconHeart)
import Control.Monad.State (class MonadState)
import Effect.Aff.Class (class MonadAff)

type Query :: forall k. k -> Type
type Query = Const Void

data Action = Toggle

type State = { on:: Boolean }

className :: forall r i. String -> HH.IProp r i
className = HH.attr (HH.AttrName "class")

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div_
    [ HH.h3_
        [ HH.text "click icon to toggle" ]
    , HH.div_
      [ iconCheck
          [ className checkClassName
          , E.onClick $ const Toggle
          ]
      , HH.text (show state.on)
      ]
    , HH.h3_
        [ HH.text "icon with hover transition" ]
    , HH.div_
      [ iconCode
        [ className "icon" ]
      , iconEye
        [ className "icon" ]
      , iconGithub
        [ className "icon" ]
      ]
    , HH.div_
      [ iconHeart [ className "icon"]
      ]
    , HH.a
      [ P.href demoSourceUrl] [HH.text "source code"]
    ]
  where
  checkClassName = if state.on then "check-icon on" else "check-icon"
  repoUrl = "https://github.com/rnons/purescript-svg-parser-halogen"
  demoSourceUrl = repoUrl <> "/tree/master/example"

component :: forall query input output m. MonadAff m => H.Component query input output m
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where

  initialState :: State
  initialState = { on: false }

handleAction :: forall m.
  MonadState State m =>
  Action -> m Unit
handleAction = case _ of
  Toggle -> do
    H.modify_ (\state -> { on: not state.on })
