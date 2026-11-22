module Main (main) where

import AST

main :: IO ()
main = print $ BinOp Add (Lit (LInt 5)) (Lit (LInt 5))
