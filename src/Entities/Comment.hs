
module Entities.Comment where

import  Data.Aeson qualified as J
import  Data.Generics.Labels qualified as GL
import Base.Create ( Create )
import Base.EmptyData ( EmptyData )
import Base.Field ( Field, Required(Optional, Required) )
import Base.Filter ( NotFiltered, CustomFilter, Filter )
import Base.Update ( Immutable, Update )
import Data.Foldable ( Foldable(fold) )
import Data.Generics.Product.Fields qualified as GL
import Data.Maybe ( catMaybes ) 
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Database.Filters
    ( rangeFilter, foreignIDsFilter, optionalFilter, applyFilter )
import Database.Utils ( (=::), (=::?) )
import Database.Types
    ( ID(..),
      DBEntity(..),
      FromDBFormat(..),
      ToDBFormat(..),
      ToDBFilter(..),
      get ) 
import Entities.Article ( Article )
import Entities.User ( User )
import Filters ( Range )
import Front ( NotAllowedFromFront, Front ) 
import GHC.Generics ( Generic )

data Comment a = Comment
  { userID    :: Field "userID"    'Required a '[Immutable]                                             (ID User)
  , articleID :: Field "articleID" 'Required a '[Immutable]                                          (ID Article)
  , content   :: Field "content"   'Required a '[CustomFilter NotFiltered]                                  Text 
  , created   :: Field "created"   'Required a '[Immutable, CustomFilter Range, NotAllowedFromFront]     UTCTime
  , modified  :: Field "modified"  'Optional a '[CustomFilter Range, NotAllowedFromFront]                UTCTime }
  deriving stock Generic

deriving instance (EmptyData (Comment Update))
deriving instance (EmptyData (Comment Filter))
deriving instance J.FromJSON (Comment (Front Create))
deriving instance J.FromJSON (Comment (Front Update))
deriving instance J.FromJSON (Comment Filter)
deriving instance J.FromJSON (Comment Update)
deriving instance J.ToJSON (Comment Create)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (Comment f) a, f ~ g, a ~ b) =>
  GL.HasField name (Comment f) (Comment g) a b
  where
  field = GL.field' @name

instance ToDBFilter Comment where
  toDBFilter comment = fold
    [ applyFilter (foreignIDsFilter userID) $ userID comment
    , applyFilter (foreignIDsFilter articleID) $ articleID comment
    , applyFilter (rangeFilter created) $ created comment
    , applyFilter (optionalFilter rangeFilter modified) $ modified comment
    ]

instance ToDBFormat (Comment Create) where
  toDBFormat comment = 
    [ userID    =:: idVal (userID comment)
    , articleID =:: idVal (articleID comment)
    , content   =:: content comment
    , created   =:: created comment
    , modified  =:: modified comment
    ]

instance ToDBFormat (Comment Update) where
  toDBFormat comment = catMaybes
    [ content   =::? content comment
    , modified  =::? modified comment
    ]

instance FromDBFormat Comment where
  fromDBFormat doc = Comment 
    <$> fmap ID (get userID doc)
    <*> fmap ID (get articleID doc)
    <*> get content doc
    <*> get created doc
    <*> get modified doc

instance DBEntity Comment where
  collection = "comment"
