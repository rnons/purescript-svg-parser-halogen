module Icons where

import Halogen.HTML (HTML, IProp)

import Svg.Renderer.Halogen (icon)

foreign import check :: String
foreign import code :: String
foreign import eye :: String
foreign import github :: String
foreign import heart :: String

type Icon = forall p r i. Array (IProp r i) -> HTML p i

iconCheck :: Icon
iconCheck = icon check

iconCode :: Icon
iconCode = icon code

iconEye :: Icon
iconEye = icon eye

iconGithub :: Icon
iconGithub = icon github

iconHeart :: Icon
iconHeart = icon heart
