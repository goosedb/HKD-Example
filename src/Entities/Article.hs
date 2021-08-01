module Entities.Article where

import Base.Create ( Create )
import Base.EmptyData ( EmptyData )
import Base.Field ( Field, Required(Optional, Required) )
import Base.Filter ( NotFiltered, ItSelf, CustomFilter, Filter )
import Base.Update ( Immutable, Update )
import Data.Aeson qualified as J
import Data.Foldable ( Foldable(fold) )
import Data.Generics.Labels qualified as GL
import Data.Generics.Product.Fields qualified as GL
import Data.List.NonEmpty ( NonEmpty, fromList, toList )
import Data.Maybe ( catMaybes )
import Data.Text ( Text )
import Data.Time
import Database.Types
    ( ID(..),
      DBEntity(..),
      FromDBFormat(..),
      ToDBFormat(..),
      ToDBFilter(..),
      get )
import Database.Utils ( (=::), (=::?) )
import Database.Filters
    ( regexFilter,
      rangeFilter,
      listFilter,
      optionalFilter,
      applyFilter,
      foreignIDsFilter )
import Database.MongoDB ((=:), lookup)
import Entities.User ( User )
import Filters ( Range, Regex )
import Front ( NotAllowedFromFront, Front )
import GHC.Generics ( Generic )


data Article a = Article
  { userID   :: Field "userID"   'Required a '[Immutable]                                     (ID User)
  , tags     :: Field "tags"     'Required a '[CustomFilter ItSelf]                     (NonEmpty Text)
  , title    :: Field "title"    'Required a '[CustomFilter Regex]                                Text
  , content  :: Field "content"  'Required a '[CustomFilter NotFiltered]                          Text 
  , created  :: Field "created"  'Required a '[CustomFilter Range, NotAllowedFromFront]        UTCTime
  , modified :: Field "modified" 'Optional a '[CustomFilter Range, NotAllowedFromFront]        UTCTime }
  deriving stock Generic


deriving instance EmptyData (Article Update)
deriving instance EmptyData (Article Filter)
deriving instance J.FromJSON (Article (Front Create))
deriving instance J.FromJSON (Article (Front Update))
deriving instance J.FromJSON (Article Filter)
deriving instance J.FromJSON (Article Update)
deriving instance J.ToJSON (Article Create)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (Article f) a, f ~ g, a ~ b) =>
  GL.HasField name (Article f) (Article g) a b
  where
  field = GL.field' @name

instance ToDBFilter Article where
  toDBFilter article = fold 
    [ applyFilter (foreignIDsFilter userID) $ userID article
    , applyFilter (listFilter tags) (toList <$> tags article)
    , applyFilter (regexFilter title) $ title article
    , applyFilter (rangeFilter created) $ created article
    , applyFilter (optionalFilter rangeFilter modified) $ modified article
    ]
    
instance ToDBFormat (Article Create) where
  toDBFormat article =
    [ userID   =:: idVal (userID article)
    , tags     =:: toList (tags article)
    , title    =:: title article
    , content  =:: content article
    , created  =:: created article
    , modified =:: modified article 
    ]

instance ToDBFormat (Article Update) where
  toDBFormat article = catMaybes
    [ tags     =::? (toList <$> tags article)
    , title    =::? title article
    , content  =::? content article
    , created  =::? created article
    , modified =::? modified article 
    ]

instance FromDBFormat Article where
  fromDBFormat doc = Article
   <$> fmap ID (get userID doc)
   <*> fmap fromList (get tags doc)
   <*> get title doc
   <*> get content doc
   <*> get created doc
   <*> get modified doc

instance DBEntity Article where
  collection = "article"
