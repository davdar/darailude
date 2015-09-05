module FP.Pretty.Color where

import FP.Prelude

newtype Color = Color { colorCode ∷ 𝕀 }
  deriving (Eq,Ord)
class ToColor a where color ∷ a → Color
instance ToColor 𝕀 where color = Color
instance ToColor ℤ where color = color ∘ 𝕚
instance ToColor ℕ where color = color ∘ 𝕚

black,darkRed,darkGreen,darkYellow,darkBlue,darkPink,darkTeal,gray ∷ Color
darkGray,red,green,yellow,blue,pink,teal,lightGray ∷ Color
white,highlight ∷ Color

black = color 0
darkRed = color 1
darkGreen = color 2
darkYellow = color 3
darkBlue = color 4
darkPink = color 5
darkTeal = color 6
gray = color 7

darkGray = color 8
red = color 9
green = color 10
yellow = color 11
blue = color 12
pink = color 13
teal = color 14
lightGray = color 15

white = color 255
highlight = color 229

