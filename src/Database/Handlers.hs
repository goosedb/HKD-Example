module Database.Handlers where

import Base.Create ( Create )
import Base.Filter ( Filter )
import Base.Schema ( nameOf )
import Base.Update ( Update )
import Control.Lens ( (.~) )
import Data.Maybe ( listToMaybe )
import Database.Filters ( idsFilter )
import Database.MongoDB ( (=:) ) 
import Database.MongoDB qualified as Mongo
import Database.Types
    ( Entity(entityID, entity),
      ID(..),
      DBEntity(..),
      FromDBFormat(fromDBFormat),
      ToDBFormat(toDBFormat),
      ToDBFilter(toDBFilter),
      WithMongo )
import Database.Utils ( queryEntity )

dbCreate :: forall e.
     WithMongo
  => DBEntity e
  => e Create -> IO (ID e)
dbCreate e = runMongo (Mongo.insert (collection @e) (toDBFormat e)) >>= (fmap ID . Mongo.cast)

dbUpdate :: forall e.
     WithMongo
  => DBEntity e
  => Entity e Update -> IO ()
dbUpdate e = runMongo $
  Mongo.modify
  (Mongo.select [ nameOf (entityID @e) =: idVal (entityID e) ] (collection @e))
  [ "$set" =: toDBFormat (entity e) ]

dbSearch :: forall e.
     WithMongo
  => DBEntity e
  => Entity e Filter -> IO [Entity e Create]
dbSearch e = do
  runMongo (Mongo.find (Mongo.select (buildQuery e) (collection @e)) >>= Mongo.rest)
    >>= traverse fromDBFormat

dbLoadByID :: forall e. 
     WithMongo 
  => DBEntity e 
  => ID e -> IO (Maybe (e Create))
dbLoadByID eid = fmap entity . listToMaybe <$> dbSearch (queryEntity @e (#entityID .~ [eid]))

dbDelete :: forall e. WithMongo => DBEntity e => ID e -> IO ()
dbDelete eID = runMongo $ Mongo.delete $
  Mongo.select [ nameOf (entityID @e) =: idVal eID ] (collection @e)

buildQuery :: forall e. DBEntity e => Entity e Filter -> Mongo.Document
buildQuery e =
  [ "$and" =: Mongo.Array [Mongo.Doc $ idsFilter (entityID e) <> toDBFilter (entity e)]
  ]

runMongo :: WithMongo => Mongo.Action IO a -> IO a
runMongo = Mongo.access ?pipe Mongo.master ?database