module Database.Filters where

import Database.Types ( WithPrefix, DBFilter, Entity(entityID), ID(ID) )
import Database.Utils ( withPrefix )
import Base.Schema ( Named, Schema )
import Base.Filter ( Exists(..) )
import Filters ( Range(Range), Regex(..) )
import Data.Maybe ( catMaybes )
import GHC.TypeLits ( KnownSymbol )
import Database.MongoDB qualified as Mongo
import Database.MongoDB ((=:))

regexFilter :: 
     WithPrefix
  => KnownSymbol s
  => (e Schema -> Named s) -> Regex a -> DBFilter 
regexFilter field (Regex a) = [ withPrefix field =: [ "$regex" =: a ] ] 

rangeFilter :: 
     WithPrefix
  => KnownSymbol s 
  => Mongo.Val a 
  => (e Schema -> Named s) -> Range a -> DBFilter
rangeFilter field (Range from to) = case catMaybes [("$gte" =:) <$> from, ("$lt" =:) <$> to] of
  [] -> []
  list -> [ withPrefix field =: list ] 

listFilter :: 
     WithPrefix
  => KnownSymbol s 
  => Mongo.Val a 
  => (e Schema -> Named s) -> [a] -> DBFilter
listFilter _ []       = []
listFilter field list = [ "$or" =: map (\val -> [ withPrefix field =: val ] ) list ] 

idsFilter ::
     WithPrefix
  => [ID a] -> DBFilter
idsFilter [] = []
idsFilter list = [ "$or" =: map (\(ID val) -> [ withPrefix entityID =: val ] ) list ] 

foreignIDsFilter ::
     WithPrefix
  => KnownSymbol s 
  => (e Schema -> Named s) -> [ID a] -> DBFilter
foreignIDsFilter field [] = []
foreignIDsFilter field list = [ "$or" =: map (\(ID val) -> [ withPrefix field =: val ] ) list ] 

optionalFilter :: WithPrefix => KnownSymbol s => ((e Schema -> Named s) -> b -> DBFilter) -> (e Schema -> Named s) -> Exists b -> DBFilter
optionalFilter f field DoesNotExist = [ withPrefix field =: Mongo.Null ]
optionalFilter f field (Exists a) = [ "$and" =: [ [ withPrefix field =: [ "$ne" =: Mongo.Null ] ] <> f field a ] ]

applyFilter :: (filter -> DBFilter) -> Maybe filter -> DBFilter
applyFilter f Nothing = []
applyFilter f (Just a) = f a

existsFilter :: WithPrefix => KnownSymbol s => (e Schema -> Named s) -> Exists a -> DBFilter
existsFilter field (Exists a) = [ withPrefix field =: [ "$ne" =: Mongo.Null ] ]
existsFilter field DoesNotExist = [ withPrefix field =: Mongo.Null ]
