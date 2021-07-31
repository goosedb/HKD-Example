{-# LANGUAGE PolyKinds #-}
module Base.Filter where

import Base.EmptyData
import Base.Field
import Base.Utils
import Data.Aeson qualified as J 
import GHC.Generics

data Filter

data CustomFilter a 

data ItSelf
data Exists a = Exists a | DoesNotExist
  deriving stock Generic 
  deriving anyclass (J.FromJSON)

newtype NotFiltered a = NotFiltered ()
  deriving stock Generic
  deriving J.FromJSON via ()

type family ApplyFilter req qs a where
  ApplyFilter req (CustomFilter q ': qs) a = Maybe (ApplyRequired req Exists (q a))
  ApplyFilter req (nq ': qs)             a = ApplyFilter req qs a
  ApplyFilter req '[]                    a = Maybe (ApplyRequired req Exists [a])  

type instance Field name req Filter modifiers a = 
  If (Contains (CustomFilter ItSelf) modifiers) 
      (Maybe (ApplyRequired req Exists a)) 
      (ApplyFilter req modifiers a)
  
query :: EmptyData (e Filter) => (e Filter -> e Filter) -> e Filter
query = ($ emptyData)