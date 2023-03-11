module TestCPT where

import SharedFunc
import Test.Tasty
import Test.Tasty.HUnit
import Datatypes
import Running
import Parsers
import CPTManipulation
import Data.Maybe (fromJust, fromMaybe)

testPrintTreeList :: TestTree
testPrintTreeList = testGroup "printTreeList tests"
  [
    testCase "printTreeList with matching input" $
      let result = printTreeList [Nbr 123, Nbr 456]
      in assertEqual "printTreeList failed to parse matching input" "a number 123, a number 456" result,
    testCase "printTreeList with non-matching input" $
      let result = printTreeList [Nbr 123, Symbol "abc"]
      in assertEqual "printTreeList parsed non-matching input" "a number 123, a symbol \"abc\"" result,
    testCase "printTreeList with empty input" $
      let result = printTreeList []
      in assertEqual "printTreeList failed to parse empty input" "" result,
    testCase "printTreeList with partially matching input" $
      let result = printTreeList [Nbr 123, Nbr 456, Symbol "abc"]
      in assertEqual "printTreeList failed to parse partially matching input" "a number 123, a number 456, a symbol \"abc\"" result,
    testCase "printTreeList with multiple matching inputs" $
      let result = printTreeList [Nbr 123, Nbr 456, Nbr 789, Nbr 101112]
      in assertEqual "printTreeList failed to parse multiple matching inputs" "a number 123, a number 456, a number 789, a number 101112" result
  ]

testPrintTree :: TestTree
testPrintTree = testGroup "printTree tests"
  [
    testCase "printTree with matching input" $
      let result = printTree (Nbr 123)
      in assertEqual "printTree failed to parse matching input" "a number 123" result,
    testCase "printTree with non-matching input" $
      let result = printTree (Symbol "abc")
      in assertEqual "printTree parsed non-matching input" "a symbol \"abc\"" result,
    testCase "printTree with empty input" $
      let result = printTree (List [])
      in assertEqual "printTree failed to parse empty input" "(a list with )" result,
    testCase "printTree with partially matching input" $
      let result = printTree (List [Nbr 123, Nbr 456, Symbol "abc"])
      in assertEqual "printTree failed to parse partially matching input" "(a list with a number 123 followed by a number 456, a symbol \"abc\")" result,
    testCase "printTree with multiple matching inputs" $
      let result = printTree (List [Nbr 123, Nbr 456, Nbr 789, Nbr 101112])
      in assertEqual "printTree failed to parse multiple matching inputs" "(a list with a number 123 followed by a number 456, a number 789, a number 101112)" result
  ]

testPrintParsedTree :: TestTree
testPrintParsedTree = testGroup "printParsedTree tests"
  [
    testCase "printParsedTree with matching input" $
      let result = printParsedTree (Just (Nbr 123, ""))
      in assertEqual "printParsedTree failed to parse matching input" "a number 123" result,
    testCase "printParsedTree with non-matching input" $
      let result = printParsedTree (Just (Symbol "abc", ""))
      in assertEqual "printParsedTree parsed non-matching input" "a symbol \"abc\"" result,
    testCase "printParsedTree with empty input" $
      let result = printParsedTree (Just (List [], ""))
      in assertEqual "printParsedTree failed to parse empty input" "an empty list" result,
    testCase "printParsedTree with partially matching input" $
      let result = printParsedTree (Just (List [Nbr 123, Nbr 456, Symbol "abc"], ""))
      in assertEqual "printParsedTree failed to parse partially matching input" "(a list with a number 123 followed by a number 456, a symbol \"abc\")" result,
    testCase "printParsedTree with multiple matching inputs" $
      let result = printParsedTree (Just (List [Nbr 123, Nbr 456, Nbr 789, Nbr 101112], ""))
      in assertEqual "printParsedTree failed to parse multiple matching inputs" "(a list with a number 123 followed by a number 456, a number 789, a number 101112)" result
  ]