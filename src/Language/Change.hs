{-|
Module     : Language.Change
Copyright  : (c) Owen Bechtel, 2023
License    : MIT
Maintainer : ombspring@gmail.com
Stability  : experimental
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Language.Change
  ( -- * Phoneme sets
    PSet(..), member
    -- * Environments
  , Pattern(..), Env(..)
  , testPatterns, testEnv
    -- * Sound changes
  , Change(..)
  , applyChange, applyChanges, traceChanges, replace
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (find, foldl', scanl')

-- | A finite set, or the complement of a finite set.
data PSet a = PSet (Set a) Bool
  deriving (Show)

-- | Test for membership in a 'PSet'.
--
-- @
-- member x (PSet set b) = 'Set.member' x set == b
-- @
member :: Ord a => a -> PSet a -> Bool
member x (PSet set b) = Set.member x set == b

-- | A single component of an 'Env'.
data Pattern a
  = One (PSet a)      -- ^ Matches one occurrence of a 'PSet' member.
  | Optional (PSet a) -- ^ Matches zero or one occurences of a 'PSet' member.
  | Many (PSet a)     -- ^ Matches zero or more occurences of a 'PSet' member.
  deriving (Show)

-- | An environment in which a phoneme (or in general, a value of type @a@), might occur.
-- An 'Env' is specified by two lists of patterns: the environment before the phoneme (ordered from nearest to farthest), and the environment after.
data Env a = Env [Pattern a] [Pattern a] 
  deriving (Show)

-- | A sound change.
newtype Change a = Change (Map a [([a], Env a)])
  deriving (Show)

-- | Match a list of phonemes against a list of patterns.
testPatterns :: Ord a => [a] -> [Pattern a] -> Bool
testPatterns list = \case
  [] -> True

  One set : ps ->
    case list of
      x:xs | member x set -> testPatterns xs ps
      _ -> False

  Optional set : ps ->
       testPatterns list ps
    || testPatterns list (One set : ps)

  Many set : ps ->
      testPatterns list ps
    || testPatterns list (One set : Many set : ps)

-- | Match two lists of phonemes against an 'Env'.
testEnv :: Ord a => [a] -> [a] -> Env a -> Bool
testEnv left right (Env psL psR) =
  testPatterns left psL && testPatterns right psR

-- | A helper function used by 'applyChange'.
-- Similar to 'map', except the first argument returns a list and has access to each element's environment.
replace :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
replace f = replace' []
  where
    replace' _ [] = []
    replace' left (x:right) = f left x right ++ replace' (x:left) right

-- | Apply a sound change to a word.
applyChange :: Ord a => Change a -> [a] -> [a]
applyChange (Change mapping) =
  replace \left x right ->
    case Map.lookup x mapping of
      Nothing -> [x]
      Just cs ->
        case find (testEnv left right . snd) cs of
          Nothing -> [x]
          Just (x', _) -> x'

-- | Apply a sequence of sound changes to a word, returning the final result.
applyChanges :: Ord a => [Change a] -> [a] -> [a]
applyChanges cs x = foldl' (flip applyChange) x cs

-- | Apply a sequence of sound changes to a word, returning a list of intermediate results.
-- (The first element of the list is the original word, and the last element is the result after applying all changes.)
traceChanges :: Ord a => [Change a] -> [a] -> [[a]]
traceChanges cs x = scanl' (flip applyChange) x cs

