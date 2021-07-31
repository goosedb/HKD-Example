module Base.Create where

import Base.Field

data Create

type instance Field name req Create modifiers a = ApplyRequired req Maybe a