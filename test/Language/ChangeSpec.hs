{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
module Language.ChangeSpec where

import Test.Hspec (Spec, it, describe, shouldBe)

import qualified Data.Set as Set
import Data.Set (Set)

import Language.Change
  ( PSet(..), member
  , Pattern(..), Env(..)
  , testPatterns, testEnv, replace
  , applyChange, applyChanges, traceChanges
  )

import Language.Change.Quote (ch, chs)

spec :: Spec
spec = do
  describe "Language.Change.member" do
    it "works on finite sets" do
      let set = PSet (Set.fromList [ 1, 5, 2 :: Int ]) True
      member 0 set `shouldBe` False
      member 4 set `shouldBe` False
      member 5 set `shouldBe` True
      member 2 set `shouldBe` True

    it "works on complements of sets" do
      let set = PSet (Set.fromList "xkcd") False
      member 'a' set `shouldBe` True
      member 'w' set `shouldBe` True
      member 'x' set `shouldBe` False
      member 'c' set `shouldBe` False 

    it "works on the empty set" do
      let set = PSet Set.empty True
      member 'a' set `shouldBe` False
      member 'z' set `shouldBe` False

    it "works on the universal set" do
      let set = PSet (Set.empty :: Set Int) False
      member 1 set `shouldBe` True
      member 9999999999999 set `shouldBe` True

  describe "Language.Change.testPatterns" do
    it "tests patterns properly" do
      let pats = 
            [ Many (PSet (Set.fromList "ab") True)
            , Optional (PSet (Set.fromList "c") True)
            , One (PSet (Set.fromList "abc") False)
            ]
      testPatterns "" pats `shouldBe` False
      testPatterns "de" pats `shouldBe` True
      testPatterns "abbbaaaabacx" pats `shouldBe` True
      testPatterns "ab" pats `shouldBe` False
      testPatterns "aaar" pats `shouldBe` True
      
  describe "Language.Change.testEnv" do
    it "tests environments properly" do
      let env = Env
            [ Many (PSet (Set.fromList "a") True) ]
            [ One (PSet (Set.fromList "r") True)
            , Optional (PSet (Set.fromList "x") True) 
            , One (PSet (Set.fromList "x") False)]
      testEnv "" "rr" env `shouldBe` True
      testEnv "aaa" "r" env `shouldBe` False
      testEnv "b" "rxa" env `shouldBe` True
      testEnv "aa" "rxx" env `shouldBe` False
  
  describe "Language.Change.replace" do
    it "replaces elements with lists" do
      let f [] _ _ = []
          f (l:_) x rs = map (\r -> r * x + l) rs 
      replace f [ 1, 3, 5, 4, 2 :: Int ] `shouldBe` [ 16, 13, 7, 23, 13, 13 ]

  let setV = "aeiou"

  describe "Language.Change.applyChange, Language.Change.Quote.ch" do
    it "applies simple changes" do
      let change1 = [ch| i > e / _i, k_ |]
      applyChange change1 "iiiii" `shouldBe` "eeeei"
      applyChange change1 "isiisi" `shouldBe` "iseisi"
      applyChange change1 "kiriitia" `shouldBe` "kereitia"

      let change2 = [ch| o > u / _V!*{ei} |]
      applyChange change2 "oi" `shouldBe` "ui"
      applyChange change2 "oki" `shouldBe` "uki"
      applyChange change2 "toronsti" `shouldBe` "torunsti"
    
    it "ignores spaces and allows comments" do
      let change1 = [ch| 
        // originally [ʏ], later [u]
        o>u/_ V ! * { e i } 
        |]
      applyChange change1 "oi" `shouldBe` "ui"
      applyChange change1 "oki" `shouldBe` "uki"
      applyChange change1 "toronsti" `shouldBe` "torunsti"

    it "applies env-split changes" do
      let change1 = [ch| { a > h; b > el; c, d > lo } / _ |]
      applyChange change1 "abc abd" `shouldBe` "hello hello"
    
    it "applies sound-split changes" do
      let change1 = [ch| a > { e / _{in}; o / _u, g_ } |]
      applyChange change1 "ga" `shouldBe` "go"
      applyChange change1 "tan" `shouldBe` "ten"
      applyChange change1 "gai" `shouldBe` "gei"
      applyChange change1 "aitaugavansarua" `shouldBe` "eitougovensarua"
    
    it "applies compound changes" do
      let change1 = [ch| 
        m, n, ŋ > { m / _{mpb}; n / _{ntd}; ŋ / _{ŋkg} }
        { p > b; t > d; k > g } / V_V
        |]
      applyChange change1 "atanpa" `shouldBe` "adampa"
      applyChange change1 "tapeŋbak" `shouldBe` "tabembak"
  
  describe "Language.Change.applyChanges, Language.Change.Quote.chs" do
    it "applies a sequence of changes (1)" do
      let changes = [chs|
        * o > u / _V!*{ei}
        * s > ʃ / _{iu}
        * u > o / _V
        * { t > d; s > z; ʃ > ʒ } / V_V
        |]

      applyChanges changes "soi" `shouldBe` "ʃoi"
      applyChanges changes "asorti" `shouldBe` "aʒurti"
      applyChanges changes "tsita" `shouldBe` "tʃida"
    
    it "applies a sequence of changes (2)" do
      let changes = [chs|
        * s > ʃ / _k{ei}
          k > % / s_{ei}
        * a > e / _ʃ
        * o > a / _u
        |]
      
      applyChanges changes "ski" `shouldBe` "ʃi"
      applyChanges changes "taski" `shouldBe` "teʃi"
      applyChanges changes "skouske" `shouldBe` "skauʃe"
  
  describe "Language.Change.traceChanges, Language.Change.Quote.chs" do
    it "returns a list of intermediate results" do
      let changes = [chs|
        * o > u / _V!*{ei}
        * s > ʃ / _{iu}
        * u > o / _V
        * { t > d; s > z; ʃ > ʒ } / V_V
        |]

      traceChanges changes "soi" `shouldBe` [ "soi", "sui", "ʃui", "ʃoi", "ʃoi" ]
      traceChanges changes "asorti" `shouldBe` [ "asorti", "asurti", "aʃurti", "aʃurti", "aʒurti" ] 
      traceChanges changes "tsita" `shouldBe` [ "tsita", "tsita", "tʃita", "tʃita", "tʃida" ]
      traceChanges changes "na" `shouldBe` replicate 5 "na" 

  

