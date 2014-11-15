module Main where

import Exercices

import Test.Hspec

main = hspec $ do
    describe "HelloWorld" $ do
        it "true equals true" $ do
            true `shouldSatisfy` (== True)
