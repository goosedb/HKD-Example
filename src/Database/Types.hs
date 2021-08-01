{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Types where

import Base.Create ( Create )
import Base.EmptyData ( EmptyData )
import Base.Filter ( Filter )
import Base.Schema ( Named, Schema, nameOf ) 
import Base.Update ( Update )
import Data.Aeson qualified as J
import Data.Aeson.Types qualified as J
import Data.Generics.Labels qualified as GL
import Data.Generics.Product.Fields qualified as GL
import Data.Text ( pack, unpack, Text ) 
import Database.MongoDB qualified as Mongo
import Database.MongoDB ( (=:) ) 
import GHC.Generics ( Generic )
import GHC.TypeLits ( KnownSymbol )
import Text.Read ( readMaybe )

type DBFilter = [Mongo.Field]
type DBDocument = Mongo.Document 

type WithMongo = (?pipe :: Mongo.Pipe, ?database :: Text)

newtype ID (a :: * -> *) = ID { idVal :: Mongo.ObjectId }
  deriving stock (Show, Read, Generic)

instance J.FromJSON (ID a) where
  parseJSON = \case 
      J.String s -> case readMaybe $ unpack s of
        Just i -> pure $ ID i
        Nothing -> err (J.String s)
      a -> err a
    where err a = J.parseFail $ "failed to parse ID. Got: " <> show a
  
instance J.ToJSON (ID a) where
  toJSON (ID a) = J.String $ pack $ show a

data Entity e a = Entity
  { entityID :: NamedID "_id" a e, entity :: e a }
  deriving stock Generic

deriving instance (J.FromJSON (e a), J.FromJSON (NamedID "_id" a e)) => J.FromJSON (Entity e a)
deriving instance (J.ToJSON (e a), J.ToJSON (NamedID "_id" a e)) => J.ToJSON (Entity e a)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (Entity e f) a, f ~ g, a ~ b) =>
  GL.HasField name (Entity e f) (Entity e g) a b
  where
  field = GL.field' @name

type family NamedID n a e :: * where
  NamedID n Schema e = Named n
  NamedID n Filter e =   [ID e]
  NamedID n a      e =    ID e

class ToDBFilter a where
  toDBFilter :: a Filter -> DBFilter

class ToDBFormat a where
  toDBFormat :: a -> DBDocument

class FromDBFormat a where
  fromDBFormat :: MonadFail m => DBDocument -> m (a Create)

instance FromDBFormat a => FromDBFormat (Entity a) where
  fromDBFormat doc = Entity 
    <$> fmap ID (get entityID doc)
    <*> fromDBFormat doc

class 
  ( FromDBFormat a
  , ToDBFormat (a Update)
  , ToDBFormat (a Create)
  , ToDBFilter a
  , EmptyData (a Update)
  , EmptyData (a Filter)
  ) => DBEntity a where
  collection :: Mongo.Collection 

get :: MonadFail m => KnownSymbol s => Mongo.Val b => (a Schema -> Named s) -> Mongo.Document -> m b
get field = Mongo.lookup (nameOf field) 