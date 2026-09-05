module Main where

import Test.Tasty
import Tests.Cases
import Tests.Lexer

main :: IO ()
main = defaultMain $ testGroup "All Tests" [lexerTests, caseTests]
