module Filters where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Data.Aeson as J

newtype Regex a = Regex Text
  deriving stock (Generic, Show)
  deriving anyclass J.FromJSON

data Range a = Range { from :: Maybe a, to :: Maybe a }
  deriving stock (Generic, Show)
  deriving anyclass J.FromJSON