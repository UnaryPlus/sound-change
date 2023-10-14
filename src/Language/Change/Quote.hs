{-|
Module     : Language.Change.Quote
Copyright  : (c) Owen Bechtel, 2023
License    : MIT
Maintainer : ombspring@gmail.com
Stability  : experimental
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
module Language.Change.Quote (ch, chs) where

import Data.Data (Data)
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..), toList, cons)
import Data.Char (isAlphaNum, isAsciiUpper, isSpace)
import Data.Bifunctor (first)
import Control.Monad (void)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

import qualified Text.Megaparsec as M
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Control.Monad.Combinators.NonEmpty as NE

import Control.Monad.Reader (ReaderT(..), local)

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote(QuasiQuoter(..), dataToExpQ)
import Language.Haskell.TH.Lib (appE, conE, varE)
import Data.Generics.Aliases (extQ)

import Language.Change (Change(..), Env(..), PSet(..), Pattern(..), applyChanges)

data CharS
  = Lit Char
  | AntiQ String -- antiquoted identifier
  | CList [Char] -- does not exist in syntax; used for compilation of antiquotes
  deriving (Data)

newtype SetS = SetS (NonEmpty CharS)
  deriving (Data)

data PSetS = PSetS SetS Bool
  deriving (Data)

data PatternS
  = OneS PSetS
  | OptionalS PSetS
  | ManyS PSetS
  deriving (Data)

data EnvS = EnvS [PatternS] [PatternS]
  deriving (Data)

type Pairing a b = NonEmpty (a, b)
type SoundList = NonEmpty CharS
type EnvList = NonEmpty EnvS
newtype Rep = Rep { convertRep :: String }
  deriving (Data)

data StatementS
  = Simple SoundList Rep EnvList
  | SoundSplit SoundList (Pairing Rep EnvList)
  | EnvSplit (Pairing SoundList Rep) EnvList 
  deriving (Data)

newtype ChangeS = ChangeS (NonEmpty StatementS)
  deriving (Data)

type Parser = ReaderT Bool (M.Parsec Void String) -- True = allow newlines

allowNewlines :: Parser a -> Parser a
allowNewlines = local (const True)

isSound, isIdentifier :: Char -> Bool
isSound c = not (isSpace c || isAsciiUpper c || c `elem` ",>/;{}[]%_!?*")
isIdentifier c = isAlphaNum c || c == '_' || c == '\''

spaces :: Parser ()
spaces = ReaderT $ \allowNs ->
  Lexer.space (if allowNs then MC.space1 else MC.hspace1) lineComment M.empty
  where lineComment = Lexer.skipLineComment "//"

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
  <$> (reverse <$> M.many patternS)
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
statementS = envSplit <|> other
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

someNewlines :: Parser ()
someNewlines = void (M.some (symbol '\n' <|> symbol '\r'))

changeS :: Parser ChangeS
changeS = ChangeS <$> NE.sepEndBy1 statementS someNewlines

bulletedChanges :: Parser (NonEmpty ChangeS)
bulletedChanges = do
  symbol '*'
  stmt <- statementS
  newline <- M.option False (True <$ someNewlines)
  if newline 
    then M.choice 
      [ (ChangeS (stmt :| []) `cons`) <$> bulletedChanges
      , (\(stmts, cs) -> ChangeS (stmt `cons` stmts) :| cs) <$> nonBulleted
      , return (ChangeS (stmt :| []) :| [])
      ]
    else return (ChangeS (stmt :| []) :| [])

  where
    nonBulleted :: Parser (NonEmpty StatementS, [ChangeS])
    nonBulleted = do
      stmt <- statementS
      newline <- M.option False (True <$ someNewlines)
      if newline
        then M.choice
          [ (\cs -> (stmt :| [], toList cs)) <$> bulletedChanges
          , first (stmt `cons`) <$> nonBulleted
          , return (stmt :| [], [])
          ]
        else return (stmt :| [], [])

total :: Parser a -> Parser a
total p = allowNewlines spaces >> (p <* M.eof)

{-|
Compile a sound change specification into a value of type 'Change' 'Char'.

Example:

@
setV = "aeiou"

change1 = [ch|
  m, n, ŋ > { m \/ _{mpb}; n \/ _{ntd}; ŋ \/ _{ŋkg} }
  { p > b; t > d; k > g } \/ V_V
  |]
@

The sound change in this example consists of two statements separated by a newline. The first statement says 
\"m, n, and ŋ become \/m\/ when followed by m, p, or b, \/n\/ when followed by n, t, or d, and \/ŋ\/ when followed by ŋ, k, or g.\"
The second statement says that voiceless stops (p, t, and k) become voiced when between two vowels. The uppercase @V@ here is
shorthand for \"any element of @setV@.\" 

All uppercase letters are interpreted in this way, which means they are not allowed as phonemes. 
Whitespace characters, as well as the following symbols ( @,>\/;{}[]%_!?*@ ) are also not allowed as phonemes. 
All other unicode characters are allowed. (For example, you can use @#@ as a \"phoneme\" representing the start or end of a word.)
 
Here's another example:

@
change2 = [ch|
  V > % \/ s_{ptk}             \/\/ vowel loss after \/s\/ before stops
  { o > ø; u > y } \/ _V!*{ji} \/\/ umlaut before \/j\/ and \/i\/ (with optional consonants in between) 
  y > i \/ _                   \/\/ unconditional shift
 |]
@

This example illustrates several features:

* You can create line comments with @\/\/@. 

* The left side of @>@ need not be a single phoneme; it can also be a set of phonemes (in this case @V@).

* The symbol @%@ represents the empty string.

* Certain suffixes can be added to phonemes and phoneme-sets to create more complex patterns:

    * @X!@ matches anything /not/ in the set @X@.

    * @X*@ matches zero or more elements of @X@.

    * @X?@ matches zero or one elements of @X@.

    * There are also the combinations @X!*@ and @X!?@.

* To create an unconditional sound change, use the empty environment @_@.

* The statements in a sound change are applied in a single traversal.
This means that the same phoneme cannot be affected by more than one statement.
For example, the change above would convert \"yni\" into \"ini,\" and \"uni\" into \"yni.\"
(If you want to sequence multiple changes, use 'chs'.)

* If multiple statements are applicable to the same phoneme, the first one is applied.
For example, the change above would convert \"soki\" into \"ski\", not \"søki.\"

Other noteworthy things:

* As mentioned above, capital letters refer to sets of characters: @V@ refers to @setV@, @N@ refers to @setN@, and so on.
To refer to sets with longer names, you can use square brackets; 
for example, if you define @setNasal :: [Char]@, you can refer to it with @[Nasal]@.

* It is possible to have multiple conditions in a single statement. For example, @e > i / _a, a_@ means \"\/e\/ becomes \/i\/ when before \/a\/ /or/ after \/a\/.\" 

* The parser is whitespace-sensitive. Horizontal space characters can go almost anywhere, 
but newlines are only allowed between statements (where they are required)
and inside of @{}@ blocks.
-}

ch :: QuasiQuoter
ch = QuasiQuoter 
  { quoteExp = makeQuoter (total changeS) (varE 'convertChangeS)
  , quotePat = undefined 
  , quoteType = undefined
  , quoteDec = undefined 
  }

{-|
Compile a sequence of sound changes into a value of type @['Change' 'Char']@.
Each change is preceded by a bullet @*@. For example:

@
changes = [chs|
  * p > f \/ _{tk}
    t > θ \/ _{pk}
    k > x \/ _{pt} \/\/ these three statements are applied in a single traversal
  * x > ç \/ i_    \/\/ this statement is applied after the ones before it
  * i > j \/ _V
  |]
@

For example, @'applyChanges' changes "ikt"@ would return @"içt"@.
-}

chs :: QuasiQuoter
chs = QuasiQuoter 
  { quoteExp = makeQuoter (toList <$> total bulletedChanges) (appE (varE 'map) (varE 'convertChangeS))
  , quotePat = undefined 
  , quoteType = undefined
  , quoteDec = undefined 
  }
  
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

makeQuoter :: (Data a) => Parser a -> TH.ExpQ -> String -> TH.ExpQ
makeQuoter parse convert input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      state = initialState input (M.SourcePos file (M.mkPos line) (M.mkPos col))
  
  case snd (M.runParser' (runReaderT parse False) state) of
    Left errors -> fail (M.errorBundlePretty errors)
    Right change -> 
      let change' = dataToExpQ (const Nothing `extQ` antiquote) change
      in appE convert change'

antiquote :: CharS -> Maybe TH.ExpQ
antiquote = \case
  Lit _ -> Nothing
  AntiQ s -> Just (appE (conE 'CList) (varE (TH.mkName ("set" ++ s))))
  CList _ -> Nothing  

badAntiQ :: a
badAntiQ = error "Language.Change.Quote: Unexpected AntiQ at runtime.\nThis error should never happen."

convertCharList :: CharS -> [Char]
convertCharList = \case
  Lit c -> [c]
  AntiQ _ -> badAntiQ
  CList cs -> cs

convertCharSet :: CharS -> Set Char
convertCharSet = \case
  Lit c -> Set.singleton c
  AntiQ _ -> badAntiQ 
  CList cs -> Set.fromList cs

convertSetS :: SetS -> Set Char
convertSetS (SetS cs) = foldMap convertCharSet cs

convertPSetS :: PSetS -> PSet Char
convertPSetS (PSetS set b) = PSet (convertSetS set) b 

convertPatternS :: PatternS -> Pattern Char
convertPatternS = \case
  OneS set -> One (convertPSetS set)
  OptionalS set -> Optional (convertPSetS set)
  ManyS set -> Many (convertPSetS set)

convertEnvS :: EnvS -> Env Char
convertEnvS (EnvS e1 e2) = Env (map convertPatternS e1) (map convertPatternS e2)

convertEnvPair :: (Rep, EnvList) -> [(String, Env Char)]
convertEnvPair (rp, envs) =
  let rp' = convertRep rp
  in map (\env -> (rp', convertEnvS env)) (toList envs)

convertStatementS :: StatementS -> [(Char, [(String, Env Char)])]
convertStatementS = \case
  Simple sounds rp envs ->
    let pairs = convertEnvPair (rp, envs) 
    in map (, pairs) (concatMap convertCharList sounds)

  SoundSplit sounds envPairs -> 
    let pairs = concatMap convertEnvPair envPairs 
    in map (, pairs) (concatMap convertCharList sounds)
    
  EnvSplit soundPairs envs ->
    let envs' = map convertEnvS (toList envs)
    in concatMap (\(sounds, rp) -> 
          let rp' = convertRep rp
              pairs = map (rp', ) envs'
          in map (, pairs) (concatMap convertCharList sounds))
        soundPairs

convertChangeS :: ChangeS -> Change Char
convertChangeS (ChangeS stmts) = Change (Map.fromListWith (flip (++)) (concatMap convertStatementS stmts))
