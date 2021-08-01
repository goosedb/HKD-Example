module Database.Utils where

import Base.EmptyData ( EmptyData(..) )
import Base.Filter ( Filter )
import Base.Schema ( Named, Schema, nameOf )
import Base.Update ( Update )
import Data.Text ( intercalate )
import Database.MongoDB qualified as Mongo
import Database.Types ( Entity(Entity), ID ) 
import GHC.TypeLits ( KnownSymbol ) 

updateEntity :: EmptyData (a Update) => ID a -> (a Update -> a Update) -> Entity a Update
updateEntity eid f = Entity eid $ f emptyData

queryEntity :: EmptyData (a Filter) => (Entity a Filter -> Entity a Filter) -> Entity a Filter
queryEntity f = f $ Entity [] emptyData

(=::) :: KnownSymbol s => Mongo.Val b => (a Schema -> Named s) -> b -> Mongo.Field
(=::) field val = nameOf field Mongo.=: val

(=::?) :: KnownSymbol s => Mongo.Val b => (a Schema -> Named s) -> Maybe b -> Maybe Mongo.Field
(=::?) field (Just val) = Just $ field =:: val
(=::?) field Nothing = Nothing
