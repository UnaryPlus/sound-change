{-|
Module     : Language.Change.Quote
Copyright  : (c) Owen Bechtel, 2023
License    : MIT
Maintainer : ombspring@gmail.com
Stability  : experimental
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Language.Change.Quote (pat, env, sim, spl) where

import qualified Text.Megaparsec as M
import qualified Control.Monad.Combinators.NonEmpty as NE
import Text.Megaparsec ((<|>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Data (Data)
import Data.Void (Void)
import Control.Monad (void)
import Data.Char (isAlphaNum, isAsciiUpper, isSpace)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ)
import Language.Haskell.TH (Q, Exp)
import Data.Generics.Aliases (extQ)

import Language.Change (Change(..), Env(..), Pattern(..), PSet(..))

type Pairing a b = NonEmpty (a, b)

data CharS
  = Lit Char
  | CSet (Set Char)
  | AntiQ String
  deriving (Data)

type SetS = NonEmpty CharS

data PSetS
  = PSetS SetS Bool
  deriving (Data)

data PatternS
  = OneS PSetS
  | OptionalS PSetS
  | ManyS PSetS
  deriving (Data)

data EnvS
  = EnvS [PatternS] [PatternS]
  deriving (Data)

data ChangeS
  = SimpleS (Pairing SetS String) (NonEmpty EnvS)
  | SplitS (Pairing SetS (Pairing String (NonEmpty EnvS)))
  deriving (Data)

type Parser = M.Parsec Void String

special :: [Char]
special = ">/,[]{}?*!_"

isSound, isIdentifier :: Char -> Bool
isSound c = not (c `elem` special || isAsciiUpper c || isSpace c)
isIdentifier c = isAlphaNum c || c == '\'' || c == '_'

spaces, spacesN :: Parser ()
spaces = void (M.takeWhileP Nothing (== ' '))
spacesN = void (M.takeWhileP Nothing isSpace)

symbol, symbolN :: Char -> Parser ()
symbol c = M.single c >> spaces
symbolN c = M.single c >> spacesN

--allows newlines
replacement :: Parser String
replacement = M.takeWhileP Nothing isSound <* spacesN

charS :: Parser CharS
charS = lit <|> oneChar <|> multipleChar
  where
    lit = Lit <$> M.satisfy isSound <* spaces

    oneChar = AntiQ . pure <$> M.satisfy isAsciiUpper <* spaces

    multipleChar = AntiQ
      <$ symbol '['
      <*> M.takeWhile1P Nothing isIdentifier
      <* spaces
      <* symbol ']'

setS :: Parser SetS
setS = (:| []) <$> charS
  <|> M.between (symbol '{') (symbol '}') (NE.some charS)

pSetS :: Parser PSetS
pSetS = do
  s <- setS
  b <- False <$ symbol '!'
    <|> return True
  return (PSetS s b)

patternS :: Parser PatternS
patternS = do
  s <- pSetS
  OptionalS s <$ symbol '?'
    <|> ManyS s <$ symbol '*'
    <|> return (OneS s)

--allows newlines
envS :: Parser EnvS
envS = EnvS
  <$> M.many patternS
  <* symbol '_'
  <*> M.many patternS
  <* spacesN

simpleS :: Parser ChangeS
simpleS = SimpleS
  <$> NE.sepBy1 change (symbolN ',')
  <* symbolN '/'
  <*> NE.sepBy1 envS (symbolN ',')
  where
    change = (,) <$> setS <* symbol '>' <*> replacement

splitS :: Parser ChangeS
splitS = SplitS <$> NE.some clause
  where
    clause = (,) <$> setS <* spacesN <*> NE.some change
    change = (,) <$ symbolN '>' <*> replacement
      <* symbolN '/' <*> NE.sepBy1 envS (symbolN ',')

--TODO: fix setLoc

setLoc :: (Int, Int) -> Parser ()
setLoc (line, col) =
  M.updateParserState \state ->
    let posState = M.statePosState state
        sourcePos = M.pstateSourcePos posState
        sourcePos' = sourcePos
          { M.sourceLine = M.mkPos line
          , M.sourceColumn = M.mkPos col
          }
        posState' = posState { M.pstateSourcePos = sourcePos' }
    in state { M.statePosState = posState' }

total :: Parser a -> Parser a
total p = spacesN >> p <* M.eof

pat :: QuasiQuoter
pat = QuasiQuoter { quoteExp = quote (M.many patternS) 'convertPatterns }

env :: QuasiQuoter
env = QuasiQuoter { quoteExp = quote envS 'convertEnv }

sim :: QuasiQuoter
sim = QuasiQuoter { quoteExp = quote simpleS 'convertChange }

spl :: QuasiQuoter
spl = QuasiQuoter { quoteExp = quote splitS 'convertChange }

quote :: Data a => Parser a -> TH.Name -> String -> Q Exp
quote p convert input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      p' = setLoc (line, col) >> total p
  case M.runParser p' file input of
    Left errors -> fail (M.errorBundlePretty errors)
    Right change -> let
      change' = dataToExpQ (const Nothing `extQ` antiquote) change
      in [| $(TH.varE convert) $(change') |]

antiquote :: CharS -> Maybe (Q Exp)
antiquote = \case
  Lit _ -> Nothing
  CSet _ -> Nothing
  AntiQ n -> Just [| $(TH.conE 'CSet) $(TH.varE (TH.mkName ("set" ++ n))) |]

convertSet :: SetS -> Set Char
convertSet = foldMap convert
  where
    convert = \case
      Lit c -> Set.singleton c
      CSet s -> s
      AntiQ _ -> undefined

convertPSet :: PSetS -> PSet Char
convertPSet (PSetS s b) = PSet (convertSet s) b

convertPattern :: PatternS -> Pattern Char
convertPattern = \case
  OneS s -> One (convertPSet s)
  OptionalS s -> Optional (convertPSet s)
  ManyS s -> Many (convertPSet s)

convertPatterns :: [PatternS] -> [Pattern Char]
convertPatterns = map convertPattern

convertEnv :: EnvS -> Env Char
convertEnv = \case
  EnvS ps1 ps2 -> Env (reverse (map convertPattern ps1)) (map convertPattern ps2)

convertChange :: ChangeS -> Change Char
convertChange = \case
  SimpleS mapping envs ->
    Simple (foldr split1 Map.empty mapping) (map convertEnv (toList envs))

  SplitS mapping ->
    Split (foldr (\(set, pairs) -> split2 (set, foldr split3 [] pairs)) Map.empty mapping)

  where
    split1 :: (SetS, a) -> Map Char a -> Map Char a
    split1 (set, x) m = foldr (\c -> Map.insert c x) m (convertSet set)

    split2 :: (SetS, [a]) -> Map Char [a] -> Map Char [a]
    split2 (set, xs) m = foldr (\c -> Map.insertWith (++) c xs) m (convertSet set)

    split3 :: Foldable t => (a, t EnvS) -> [(a, Env Char)] -> [(a, Env Char)]
    split3 (x, envs) m = foldr (\e -> ((x, convertEnv e) :)) m envs
