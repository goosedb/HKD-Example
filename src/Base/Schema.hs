module Base.Schema where


import Base.Field
import Data.Proxy
import Data.Text 
import GHC.TypeLits

data Schema

data Named (n :: Symbol)

type instance Field name req Schema modifiers a = Named name

nameOf :: forall e n a. KnownSymbol n => (e Schema -> Named n) -> Text 
nameOf _ = pack $ symbolVal (Proxy @n)