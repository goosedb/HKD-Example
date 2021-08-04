{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
module Base.Filter where

import Base.EmptyData
import Base.Field
import Base.Utils
import Data.Aeson qualified as J 
import Data.Aeson.Types qualified as J 
import GHC.Generics

data Filter

data CustomFilter a 

data ItSelf
data Exists a = Exists a | DoesNotExist
  deriving stock (Generic, Show)

instance {-# OVERLAPPING #-} J.FromJSON (Exists (NotFiltered a)) where
  parseJSON = parseExists (const $ pure NotFiltered)

instance {-# OVERLAPPABLE #-} J.FromJSON a => J.FromJSON (Exists a) where 
  parseJSON = parseExists J.parseJSON

parseExists :: (J.Value -> J.Parser a) -> J.Value -> J.Parser (Exists a)
parseExists f = J.withObject "Exists" \obj -> do
    obj J..: "tag" >>= \case
      "Exists" -> Exists <$> ((obj J..: "content") >>= f)
      "DoesNotExist" ->  pure DoesNotExist
      s -> J.unexpected $ J.String s

data NotFiltered a = NotFiltered
  deriving stock (Generic, Show)

instance J.FromJSON (NotFiltered a) where
  parseJSON _ = fail "Can't filter on this field"

type family ApplyFilter req qs a where
  ApplyFilter req (CustomFilter q ': qs) a = Maybe (ApplyRequired req Exists (q a))
  ApplyFilter req (nq ': qs)             a = ApplyFilter req qs a
  ApplyFilter req '[]                    a = Maybe (ApplyRequired req Exists [a])  

type instance Field name req Filter modifiers a = 
  If (Contains (CustomFilter ItSelf) modifiers) 
      (Maybe (ApplyRequired req Exists a)) 
      (ApplyFilter req modifiers a)
  
query :: EmptyData (e Filter) => (e Filter -> e Filter) -> e Filter
query = ($ emptyData)