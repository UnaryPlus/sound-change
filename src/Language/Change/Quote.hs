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
module Language.Change.Quote where

import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Char (isAlphaNum, isAsciiUpper, isSpace)
import Control.Monad (void)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Control.Monad.Combinators.NonEmpty as NE

import Control.Monad.Reader (ReaderT(..), local)

{-
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
-}

{-
ENVIRONMENTS
set1c1set2c2_set3c3set4c4
set = unit | {unitunitunit}
unit = single phoneme | antiquote
c = optional ! followed by optional ? or *

SIMPLE CHANGES
p1, p2 > s / env1, env2 

PHONEME SPLITTING
p1, p2 > { s1 / env1; s2 / env2, env3 }

ENVIRONMENT SPLITTING
{ p1 > s1; p2, p3 > s2 } / env1, env2

EMPTY STRING
use % for the empty replacement

ANTIQUOTING CHARSETS
capital letter, or sequence of identifier characters (alphanumeric, _, ') in square brackets [ ]
can occur in phoneme lists, or in environments

NEWLINES
newlines are required between statements
within statements, they are allowed anywhere within { }

SPACES
spaces are allowed anywhere (except in antiquoted identifiers)

ALLOWED PHONEMES
everything except whitespace, capital letters, and reserved symbols (, > / ; { } [ ] % _ ! ? *) 
in particular, you can use # for word boundaries, : for long vowels, etc.
(these are treated just like any phoneme)

COMMENTS
line comments begin with // 

MULTIPLE STATEMENTS
no restrictions on which types of statements can occur together
but note that in case of conflicting cases (same phoneme, multiple matching environments) the first rule will be applied
-}

data CharS
  = Lit Char
  | AntiQ String -- antiquoted identifier
  | CList [Char] -- does not exist in syntax; used for compilation of antiquotes

newtype SetS = SetS (NonEmpty CharS)

data PSetS = PSetS SetS Bool

data PatternS
  = OneS PSetS
  | OptionalS PSetS
  | ManyS PSetS

data EnvS = EnvS [PatternS] [PatternS]

type Pairing a b = NonEmpty (a, b)
type SoundList = NonEmpty CharS
type EnvList = NonEmpty EnvS
newtype Rep = Rep String

data StatementS
  = Simple SoundList Rep EnvList
  | SoundSplit SoundList (Pairing Rep EnvList)
  | EnvSplit (Pairing SoundList Rep) EnvList 

type ChangeS = NonEmpty StatementS

type Parser = ReaderT Bool (M.Parsec Void String) -- True = allow newlines

allowNewlines :: Parser a -> Parser a
allowNewlines = local (const True)

isSound, isIdentifier :: Char -> Bool
isSound c = not (isSpace c || isAsciiUpper c || c `elem` ",>/;{}[]%_!?*")
isIdentifier c = isAlphaNum c || c == '_' || c == '\''

spaces :: Parser ()
spaces = ReaderT $ \allowNs ->
  Lexer.space (if allowNs then spsN else sps) lineComment M.empty
  where 
    sps = void (M.takeWhile1P (Just "space") (== ' '))
    spsN = void (M.takeWhile1P (Just "white space") isSpace)
    lineComment = Lexer.skipLineComment "//"

symbol :: Char -> Parser ()
symbol c = M.single c >> spaces

charS :: Parser CharS
charS = M.choice [ lit, oneChar, multipleChar ]
  where
    lit = Lit <$> M.satisfy isSound <* spaces

    oneChar = AntiQ . pure <$> M.satisfy isAsciiUpper <* spaces

    multipleChar = AntiQ
      <$ symbol '['
      <*> M.takeWhile1P (Just "Haskell identifier") isIdentifier
      <* spaces
      <* symbol ']'

setS :: Parser SetS
setS = M.choice
  [ SetS . pure <$> charS
  , SetS <$ symbol '{' <*> NE.some charS <* symbol '}'
  ]

pSetS :: Parser PSetS
pSetS = PSetS 
  <$> setS 
  <*> M.choice [ False <$ symbol '!', return True ]

patternS :: Parser PatternS
patternS = do
  pset <- pSetS
  M.choice
    [ OptionalS pset <$ symbol '?'
    , ManyS pset <$ symbol '*'
    , return (OneS pset)
    ]

envS :: Parser EnvS
envS = EnvS
  <$> M.many patternS
  <* symbol '_'
  <*> M.many patternS

pairing :: Parser a -> Parser () -> Parser b -> Parser (Pairing a b)
pairing pa psep pb = do
  allowNewlines (symbol '{')
  ps <- allowNewlines (NE.sepBy1 pair (symbol ';'))
  symbol '}'
  return ps
  where
    pair = (,) <$> pa <* psep <*> pb

soundList :: Parser SoundList
soundList = NE.sepBy1 charS (symbol ',')

envList :: Parser EnvList
envList = NE.sepBy1 envS (symbol ',')

rep :: Parser Rep
rep = M.choice
  [ Rep <$> M.takeWhile1P (Just "phoneme") isSound <* spaces
  , Rep "" <$ symbol '%'
  ]

statementS :: Parser StatementS
statementS = M.choice [ envSplit, other ]
  where
    envSplit = EnvSplit 
      <$> pairing soundList (symbol '>') rep 
      <* symbol '/' 
      <*> envList
    
    other = do
      sounds <- soundList
      symbol '>'
      M.choice
        [ SoundSplit sounds <$> pairing rep (symbol '/') envList
        , Simple sounds <$> rep <* symbol '/' <*> envList
        ]

changeS :: Parser ChangeS
changeS = NE.sepBy1 statementS (symbol '\n')

initialState :: s -> M.SourcePos -> M.State s Void
initialState input position =
  M.State
    { M.stateInput = input
    , M.stateOffset = 0
    , M.statePosState =
        M.PosState
          { M.pstateInput = input
          , M.pstateOffset = 0
          , M.pstateSourcePos = position
          , M.pstateTabWidth = M.defaultTabWidth
          , M.pstateLinePrefix = ""
          }
    , M.stateParseErrors = []
    }




{-
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
-}