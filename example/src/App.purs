module App where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Icons (iconCheck, iconCode, iconEye, iconGithub, iconHeart)

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
          , E.onClick $ Just <<< const Toggle
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

app :: forall m. H.Component HH.HTML Query Unit Void m
app = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      }
  }
  where

  initialState :: State
  initialState = { on: false }

  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    Toggle -> do
      H.modify_ (\state -> { on: not state.on })
