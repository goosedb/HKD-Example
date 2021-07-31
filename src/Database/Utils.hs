module Database.Utils where

import Base.EmptyData ( EmptyData(..) )
import Base.Filter ( Filter )
import Base.Schema ( Named, Schema, nameOf )
import Base.Update ( Update )
import Data.Text ( intercalate )
import Database.MongoDB qualified as Mongo
import Database.Types ( WithPrefix, Entity(Entity), ID ) 
import GHC.TypeLits ( KnownSymbol ) 

withPrefix :: KnownSymbol s => WithPrefix => (a Schema -> Named s) -> Mongo.Label
withPrefix l = intercalate "." ?prefix <> nameOf l

addPrefix :: WithPrefix => Mongo.Label -> (WithPrefix => a) -> a
addPrefix p = let ?prefix = ?prefix <> [p] in id

updateEntity :: EmptyData (a Update) => ID a -> (a Update -> a Update) -> Entity a Update
updateEntity eid f = Entity eid $ f emptyData

queryEntity :: EmptyData (a Filter) => (Entity a Filter -> Entity a Filter) -> Entity a Filter
queryEntity f = f $ Entity [] emptyData
