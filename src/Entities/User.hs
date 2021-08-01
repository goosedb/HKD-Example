
module Entities.User where


import Base.Create ( Create )
import Base.EmptyData ( EmptyData )
import Base.Field ( Field, Required(Optional, Required) )
import Base.Filter ( CustomFilter, Filter, NotFiltered )
import Base.Update ( Update, Immutable )
import Data.Aeson qualified as J
import Data.Foldable ( Foldable(fold) )
import Data.Generics.Labels qualified as GL
import Data.Generics.Product.Fields qualified as GL
import Data.Maybe ( catMaybes )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Database.Filters
    ( rangeFilter,
      listFilter,
      optionalFilter,
      applyFilter,
      existsFilter )
import Database.Utils ( (=::), (=::?) )
import Database.MongoDB ((=:))
import Database.Types
    ( DBEntity(..),
      FromDBFormat(..),
      ToDBFormat(..),
      ToDBFilter(..),
      get )
import Filters ( Range )
import Front ( NotAllowedFromFront, Front )
import GHC.Generics ( Generic )


data User a = User
  { registered :: Field "registered" 'Required a '[Immutable, CustomFilter Range, NotAllowedFromFront] UTCTime
  , modified   :: Field "modified"   'Optional a '[CustomFilter Range, NotAllowedFromFront]            UTCTime 
  , login      :: Field "login"      'Required a '[]                                                      Text
  , email      :: Field "email"      'Optional a '[]                                                      Text
  , about      :: Field "about"      'Optional a '[CustomFilter NotFiltered]                              Text }
  deriving stock Generic

deriving instance EmptyData (User Update)
deriving instance EmptyData (User Filter)
deriving instance J.FromJSON (User (Front Create))
deriving instance J.FromJSON (User (Front Update))
deriving instance J.FromJSON (User Filter)
deriving instance J.FromJSON (User Update)
deriving instance J.ToJSON (User Create)

instance
  {-# OVERLAPPING #-}
  (GL.HasField' name (User f) a, f ~ g, a ~ b) =>
  GL.HasField name (User f) (User g) a b
  where
  field = GL.field' @name

instance ToDBFilter User where
  toDBFilter user = fold
    [ applyFilter (rangeFilter registered) (registered user) 
    , applyFilter (optionalFilter rangeFilter modified) (modified user)
    , applyFilter (listFilter login) (login user)
    , applyFilter (optionalFilter listFilter email) (email user)
    , applyFilter (existsFilter about) (about user) ]

instance ToDBFormat (User Create) where
  toDBFormat user = 
    [ registered =:: registered user
    , modified   =:: modified   user
    , login      =:: login      user
    , email      =:: email      user
    , about      =:: about      user 
    ]

instance ToDBFormat (User Update) where
  toDBFormat user = catMaybes
    [ modified   =::? modified   user
    , login      =::? login      user
    , email      =::? email      user
    , about      =::? about      user 
    ]

instance FromDBFormat User where
  fromDBFormat doc = User
    <$> get registered doc
    <*> get modified doc
    <*> get login doc
    <*> get email doc
    <*> get about doc

instance DBEntity User where
  collection = "user"
