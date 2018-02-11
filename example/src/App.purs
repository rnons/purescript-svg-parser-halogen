module App where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Icons (iconCheck, iconCode, iconEye, iconGithub, iconHeart)

data Query a = Toggle a

type State = { on:: Boolean }

className :: forall r i. String -> HH.IProp r i
className = HH.attr (HH.AttrName "class")

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.h3_
        [ HH.text "click icon to toggle" ]
    , HH.div_
      [ iconCheck
          [ className checkClassName
          , E.onClick (E.input_ Toggle)
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
  demoSourceUrl = repoUrl <> "/tree/gh-pages"

app :: forall m. H.Component HH.HTML Query Unit Void m
app =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { on: false }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Toggle next -> do
      H.modify (\state -> { on: not state.on })
      pure next
