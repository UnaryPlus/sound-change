{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Language.Change
  ( PSet(..), member
  , Pattern(..), Env(..), Change(..)
  , testPatterns, testEnv, replace
  , applyChange, applyChanges, traceChanges
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Map (Map)
import Data.List (find, foldl', scanl')

data PSet a
  = PSet (Set a) Bool

member :: Ord a => a -> PSet a -> Bool
member x (PSet set b) =
  if b then Set.member x set else Set.notMember x set

data Pattern a
  = One (PSet a)
  | Optional (PSet a)
  | Many (PSet a)

data Env a
  = Env [Pattern a] [Pattern a]

data Change a
  = Simple (Map a [a]) [Env a]
  | Split (Map a [([a], Env a)])


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


testEnv :: Ord a => [a] -> [a] -> Env a -> Bool
testEnv left right (Env psL psR) =
  testPatterns left psL && testPatterns right psR


replace :: ([a] -> a -> [a] -> [b]) -> [a] -> [b]
replace f = replace' []
  where
    replace' _ [] = []
    replace' left (x:right) = f left x right ++ replace' (x:left) right


applyChange :: Ord a => Change a -> [a] -> [a]
applyChange = \case
  Simple mapping envs -> replace \left x right ->
    case Map.lookup x mapping of
      Just x' | any (testEnv left right) envs -> x'
      _ -> [x]

  Split mapping -> replace \left x right ->
    case Map.lookup x mapping of
      Nothing -> [x]
      Just cs ->
        case find (testEnv left right . snd) cs of
          Nothing -> [x]
          Just (x', _) -> x'

applyChanges :: Ord a => [Change a] -> [a] -> [a]
applyChanges cs x = foldl' (flip applyChange) x cs

traceChanges :: Ord a => [Change a] -> [a] -> [[a]]
traceChanges cs x = scanl' (flip applyChange) x cs
