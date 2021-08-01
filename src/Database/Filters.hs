module Database.Filters where

import Database.Types ( DBFilter, Entity(entityID), ID(ID) )
import Database.Utils ( (=::) )
import Base.Schema ( Named, Schema )
import Base.Filter ( Exists(..) )
import Filters ( Range(Range), Regex(..) )
import Data.Maybe ( catMaybes )
import GHC.TypeLits ( KnownSymbol )
import Database.MongoDB qualified as Mongo
import Database.MongoDB ((=:))

regexFilter :: 
     KnownSymbol s
  => (e Schema -> Named s) -> Regex a -> DBFilter 
regexFilter field (Regex a) = [ field =:: [ "$regex" =: a ] ] 

rangeFilter :: 
     KnownSymbol s 
  => Mongo.Val a 
  => (e Schema -> Named s) -> Range a -> DBFilter
rangeFilter field (Range from to) = case catMaybes [("$gte" =:) <$> from, ("$lt" =:) <$> to] of
  [] -> []
  list -> [ field =:: list ] 

listFilter :: 
     KnownSymbol s 
  => Mongo.Val a 
  => (e Schema -> Named s) -> [a] -> DBFilter
listFilter _ []       = []
listFilter field list = [ "$or" =: map (\val -> [ field =:: val ] ) list ] 

idsFilter :: [ID a] -> DBFilter
idsFilter [] = []
idsFilter list = [ "$or" =: map (\(ID val) -> [ entityID =:: val ] ) list ] 

foreignIDsFilter ::
     KnownSymbol s 
  => (e Schema -> Named s) -> [ID a] -> DBFilter
foreignIDsFilter field [] = []
foreignIDsFilter field list = [ "$or" =: map (\(ID val) -> [ field =:: val ] ) list ] 

optionalFilter :: KnownSymbol s => ((e Schema -> Named s) -> b -> DBFilter) -> (e Schema -> Named s) -> Exists b -> DBFilter
optionalFilter f field DoesNotExist = [ field =:: Mongo.Null ]
optionalFilter f field (Exists a) = [ "$and" =: [ [ field =:: [ "$ne" =: Mongo.Null ] ] <> f field a ] ]

applyFilter :: (filter -> DBFilter) -> Maybe filter -> DBFilter
applyFilter f Nothing = []
applyFilter f (Just a) = f a

existsFilter :: KnownSymbol s => (e Schema -> Named s) -> Exists a -> DBFilter
existsFilter field (Exists a) = [ field =:: [ "$ne" =: Mongo.Null ] ]
existsFilter field DoesNotExist = [ field =:: Mongo.Null ]
