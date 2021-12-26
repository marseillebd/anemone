{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.List.Reverse
  ( RList
  -- * Introduction and Elimination
  , nil
  , snoc
  , singleton
  , unsnoc
  -- ** Patterns
  , pattern Nil
  , pattern Snoc
  -- * Queries
  , null
  -- * Traversal
  , catMaybes
  -- * Conversion
  , toList
  , toSet
  , reverse
  ) where

import Prelude hiding (null,reverse)

import Control.DeepSeq (NFData)
import Data.Set (Set)
import GHC.Generics (Generic)

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Prelude


newtype RList a = RList { unRList :: [a] }
  deriving stock (Show,Generic)
  deriving newtype (Functor,Foldable)
instance (NFData a) => NFData (RList a)

nil :: RList a
nil = RList []

snoc :: RList a -> a -> RList a
snoc (RList xs) x = RList (x:xs)

unsnoc :: RList a -> Maybe (RList a, a)
unsnoc (RList []) = Nothing
unsnoc (RList (x:xs)) = Just (RList xs, x)

{-# COMPLETE Nil, Snoc #-}

pattern Nil :: RList a
pattern Nil = RList []

pattern Snoc :: RList a -> a -> RList a
pattern Snoc xs x <- (unsnoc -> Just (xs, x))
  where Snoc = snoc

singleton :: a -> RList a
singleton = RList . (:[])

null :: RList a -> Bool
null (RList xs) = List.null xs

catMaybes :: RList (Maybe a) -> RList a
catMaybes = RList . Maybe.catMaybes . unRList

toList :: RList a -> [a]
toList (RList xs) = Prelude.reverse xs

toSet :: (Ord a) => RList a -> Set a
toSet (RList xs) = Set.fromList xs

reverse :: RList a -> [a]
reverse = unRList
