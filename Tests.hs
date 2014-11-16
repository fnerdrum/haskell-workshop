module Main where

import Functions
import Lists
import ListOperations

import Test.Hspec

main = hspec $ do
  describe "Funksjoner.myName" $ do
    it "Skal ha et navn" $ do
      myName `shouldSatisfy` (> "")

  describe "Funksjoner.doubleMe" $ do
    it "Skal doble et tall" $ do
      doubleMe 1 `shouldBe` 2

  describe "Funksjoner.doubleUs" $ do
    it "Skal doble to tall" $ do
      doubleUs 1 2 `shouldBe` 6

  describe "Lists.positive" $ do
    it "Kan håndtere tomme lister" $ do
      positive [] `shouldBe` []
    it "Skal kun beholde positive tall" $ do
      positive [1,-1,0,3] `shouldBe` [1,3]

  describe "Lists.flatten" $ do
    it "Kan flate ut nestede lister" $ do
      flatten [[1,2],[3],[]] `shouldBe` [1,2,3]

  describe "Lists.filterNot" $ do
    it "Kan håndtere tomme lister" $ do
      filterNot (>0) [] `shouldBe` []
    it "Beholder kun ønskede elementer" $ do
      filterNot (>0) [1,-1,2,-2] `shouldBe` [-1,-2]

  describe "ListOperations.filter'" $ do
    it "Kan håndtere tomme lister" $ do
      filter' (>0) [] `shouldBe` []
    it "Beholder kun ønskede elementer" $ do
      filter' (>0) [1,-1,2,-2] `shouldBe` [1,2]

  describe "ListOperations.map'" $ do
    it "Kan håndtere tomme lister" $ do
      map' (+1) [] `shouldBe` []
    it "Legger til 1 for alle elementene" $ do
      map' (+1) [1,-1,2,-2] `shouldBe` [2,0,3,-1]

  describe "ListOperations.head'" $ do
    it "Henter ut det første elementet i listen" $ do
      head'  [1..] `shouldBe` 1

  describe "ListOperations.tail'" $ do
    it "Fjerner det første elementet i listen" $ do
      tail'  [1..10] `shouldBe` [2..10]

      
  describe "ListOperations.sum'" $ do
    it "Håndterer tomme lister " $ do
      sum'  [] `shouldBe` 0
    it "Summerer alle elementene i listen" $ do
      sum' [1..3] `shouldBe` 6

  describe "ListOperations.repeat'" $ do
    it "Repeterer uendelig mange ganger" $ do
      (take 1000 $ repeat' 1) `shouldBe` (take 1000 $ repeat 1)

  describe "ListOperations.take'" $ do
    it "Ønsker 0 elementer gir tom liste" $ do
      (take' 0 [1..]) `shouldBe` []
    it "Ønsker flere elementer enn det som er tilgjengelig" $ do
      (take' 2 [1]) `shouldBe` [1]
    it "Beholder ønsket antall elementer" $ do
      (take' 10 [1..]) `shouldBe` [1..10]

  describe "ListOperations.zipWith'" $ do
    it "Første liste har flere elementer enn andre liste" $ do
      (zipWith' (+) [1..] [1]) `shouldBe` [2]
    it "Andre liste har flere elementer enn første liste" $ do
      (zipWith' (+) [1] [1..]) `shouldBe` [2]

  describe "ListOperations.zip'" $ do
    it "Første liste har flere elementer enn andre liste" $ do
      (zip' [1..] [1]) `shouldBe` [(1,1)]
    it "Andre liste har flere elementer enn første liste" $ do
      (zip' [1] [1..]) `shouldBe` [(1,1)]
    it "Kombinerer elementene i listene" $ do
      (zip' [1..2] [1..2]) `shouldBe` [(1,1),(2,2)]

  describe "ListOperations.index" $ do
    it "Indekserer elementene i lister" $ do
      index ['a','b','c'] `shouldBe` [(1,'a'),(2,'b'),(3,'c')]

  describe "ListOperations.fold" $ do
    it "Håndterer tomme lister" $ do
      (fold (+) 0 []) `shouldBe` 0
    it "Legger sammen elementene i listen" $ do
      (fold (+) 0 [1..3]) `shouldBe` 6

  describe "ListOperations.elem'" $ do
    it "Håndterer tomme lister" $ do
      (elem' 1 []) `shouldBe` False
    it "Listen inneholder elementet" $ do
      (elem' 1 [1..]) `shouldBe` True
    it "Listen inneholder ikke elementet" $ do
      (elem' 1 [2..10]) `shouldBe` False

  describe "ListOperations.reverse'" $ do
    it "Reverserer elementene i listen" $ do
      (reverse' [1..10]) `shouldBe` [10,9..1]

  describe "ListOperations.maximum'" $ do
    it "Returnerer det største elementet i listen" $ do
      (maximum' [1,10,5,0]) `shouldBe` 10

  describe "ListOperations.takeWhile'" $ do
    it "Håndterer tomme lister" $ do
      (takeWhile' (>0) []) `shouldBe` []
    it "Returnerer elementer så lenge predikatet er sant" $ do
      (takeWhile' (<5) [1..10]) `shouldBe` [1..4]
