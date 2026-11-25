module Main where

import Test.Hspec
import Types
import Helpers (tv)


main :: IO ()
main = hspec $ do
    describe "Types.prettyPrint" $ do
        it "nests to the right without brackets" $ do
            prettyPrint (TArr TBool (TArr TInt TBool)) `shouldBe` "Bool -> Int -> Bool"
            prettyPrint (TArr (tv "d") (TArr (tv "c") (TArr (tv "a") (tv "b")))) `shouldBe` "d -> c -> a -> b"
        it "prints arg functions with left-associated brackets" $ do
            prettyPrint (TArr (TArr TInt TBool) TBool) `shouldBe` "(Int -> Bool) -> Bool"
            prettyPrint (TArr (TArr (tv "c") (TArr (tv "a") (tv "b"))) (tv "d") ) `shouldBe` "(c -> a -> b) -> d"

