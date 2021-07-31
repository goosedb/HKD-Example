
module Front where

import Base.Field ( Field )
import Base.Utils ( If, Contains )
import Data.Aeson qualified as J 

data Front a
data NotAllowedFromFront

instance J.FromJSON NotAllowedFromFront where
  parseJSON _ = fail "Can't specify this field"

type instance Field name req (Front b) modifiers a =
  If (Contains NotAllowedFromFront modifiers) 
    (Maybe NotAllowedFromFront)
    (Field name req b modifiers a)

