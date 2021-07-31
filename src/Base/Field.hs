module Base.Field where

import GHC.TypeLits

data Required = Required | Optional

type family ApplyRequired (req :: Required) m a where
  ApplyRequired 'Required m a = a
  ApplyRequired 'Optional m a = m a

type family Field (name :: Symbol) (req :: Required) action (modifiers :: [*]) a :: *