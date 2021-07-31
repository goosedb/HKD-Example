module Base.Utils where

type family Contains a as where
  Contains a (a ': as) = 'True
  Contains b (a ': as) = Contains b as
  Contains a '[]       = 'False

type family If c t e where  
  If 'True t e = t
  If 'False t e = e