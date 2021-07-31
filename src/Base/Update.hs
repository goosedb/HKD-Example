module Base.Update where

import Base.EmptyData
import Base.Field
import Base.Utils
import Data.Aeson qualified as J

data Update

data Immutable
data NotUpdated

instance J.FromJSON NotUpdated where
  parseJSON _ = fail "Can't update this field"

type instance Field name req Update modifiers a = 
  If (Contains Immutable modifiers) (Maybe (ApplyRequired req Maybe NotUpdated)) (Maybe (ApplyRequired req Maybe a))

update :: EmptyData (e Update) => (e Update -> e Update) -> e Update
update = ($ emptyData) 
