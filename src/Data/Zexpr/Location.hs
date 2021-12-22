module Data.Zexpr.Location
  ( Loc(..)
  ) where

data Loc
  = LocUnknown
  | Loc
    { filename :: FilePath -- an empty string should be able to take on the fucntion of sentinel value
                           -- after all, no file is identified by an empty filepath
                           -- also, megaparsec _really_ wants a filename, so XP
    , fromLine :: Int
    , fromCol :: Int
    , toLine :: Int
    , toCol :: Int
    }
  deriving(Show,Eq)

instance Semigroup Loc where
  l1 <> l2 = Loc
    { filename = filename l1
    , fromLine = fromLine l1
    , fromCol = fromCol l1
    , toLine = toLine l2
    , toCol = toCol l2
    }
