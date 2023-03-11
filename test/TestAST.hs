module TestAST where

import SharedFunc
import Test.Tasty
import Test.Tasty.HUnit
import Datatypes
import Running
import Parsers
import ASTManipulation

testCptToAST :: TestTree
testCptToAST = testGroup "cptToAST tests"
  [
    testCase "cptToAST with matching input" $
      let result = cptToAST (Symbol "abc")
      in assertEqual "cptToAST failed to parse matching input" (Just (Ref "abc")) result,
    testCase "cptToAST with non-matching input" $
      let result = cptToAST (Symbol "abc")
      in assertEqual "cptToAST parsed non-matching input" (Just (Ref "abc")) result,
    testCase "cptToAST with empty input" $
      let result = cptToAST (List [])
      in assertEqual "cptToAST failed to parse empty input" Nothing result,
    testCase "cptToAST with partially matching input" $
      let result = cptToAST (List [Symbol "abc", Symbol "=", Symbol "def"])
      in assertEqual "cptToAST failed to parse partially matching input" (Just (Define "abc" (Ref "def"))) result,
    testCase "cptToAST with multiple matching inputs" $
      let result = cptToAST (List [Symbol "abc", Symbol "=", Symbol "def", Symbol "=", Symbol "ghi"])
      in assertEqual "cptToAST failed to parse multiple matching inputs" (Just (Call (Ref "abc") [Ref "=",Ref "def",Ref "=",Ref "ghi"])) result
  ]

testAddAst :: TestTree
testAddAst = testGroup "addAst tests"
  [
    testCase "addAst with matching input" $
      let result = addAst (Val 123) (Val 456)
      in assertEqual "addAst failed to parse matching input" (Just (Val 579)) result,
    testCase "addAst with non-matching input" $
      let result = addAst (Val 123) (Ref "abc")
      in assertEqual "addAst parsed non-matching input" Nothing result,
    testCase "addAst with empty input" $
      let result = addAst (Val 0) (Val 0)
      in assertEqual "addAst failed to parse empty input" (Just (Val 0)) result,
    testCase "addAst with invalid input" $
      let result = addAst (Boolean True) (Ref "abc")
      in assertEqual "addAst parsed invalid input" Nothing result,
    testCase "addAst with matching input with Procedure type" $
      let result = addAst (Procedure [] (Val 123) Nothing) (Procedure [] (Val 456) Nothing)
      in assertEqual "addAst failed to parse matching input with Procedure type" Nothing result,
    testCase "addAst with matching input with Lambda type" $
      let result = addAst (Lambda (Val 0) [] (Val 123)) (Lambda (Val 0) [] (Val 456))
      in assertEqual "addAst failed to parse matching input with Lambda type" Nothing result
  ]

testSubAst :: TestTree
testSubAst = testGroup "subAst tests"
  [
    testCase "subAst with matching input" $
      let result = subAst (Val 123) (Val 456)
      in assertEqual "subAst failed to parse matching input" (Just (Val (-333))) result,
    testCase "subAst with non-matching input" $
      let result = subAst (Val 123) (Ref "abc")
      in assertEqual "subAst parsed non-matching input" Nothing result,
    testCase "subAst with empty input" $
      let result = subAst (Val 123) (Val 456)
      in assertEqual "subAst failed to parse empty input" (Just (Val (-333))) result,
    testCase "subAst with partially matching input" $
      let result = subAst (Val 123) (Val 456)
      in assertEqual "subAst failed to parse partially matching input" (Just (Val (-333))) result,
    testCase "subAst with multiple matching inputs" $
      let result = subAst (Val 123) (Val 456)
      in assertEqual "subAst failed to parse multiple matching inputs" (Just (Val (-333))) result
  ]

testMulAst :: TestTree
testMulAst = testGroup "mulAst tests"
  [
    testCase "mulAst with matching input" $
      let result = mulAst (Val 123) (Val 456)
      in assertEqual "mulAst failed to parse matching input" (Just (Val 56088)) result,
    testCase "mulAst with non-matching input" $
      let result = mulAst (Val 123) (Ref "abc")
      in assertEqual "mulAst parsed non-matching input" Nothing result,
    testCase "mulAst with empty input" $
      let result = mulAst (Val 123) (Val 456)
      in assertEqual "mulAst failed to parse empty input" (Just (Val 56088)) result,
    testCase "mulAst with partially matching input" $
      let result = mulAst (Val 123) (Val 456)
      in assertEqual "mulAst failed to parse partially matching input" (Just (Val 56088)) result,
    testCase "mulAst with multiple matching inputs" $
      let result = mulAst (Val 123) (Val 456)
      in assertEqual "mulAst failed to parse multiple matching inputs" (Just (Val 56088)) result
  ]

testDivAst :: TestTree
testDivAst = testGroup "divAst tests"
  [
    testCase "divAst with matching input" $
      let result = divAst (Val 123) (Val 456)
      in assertEqual "divAst failed to parse matching input" (Just (Val 0)) result,
    testCase "divAst with non-matching input" $
      let result = divAst (Val 123) (Ref "abc")
      in assertEqual "divAst parsed non-matching input" Nothing result,
    testCase "divAst with empty input" $
      let result = divAst (Val 123) (Val 456)
      in assertEqual "divAst failed to parse empty input" (Just (Val 0)) result,
    testCase "divAst with partially matching input" $
      let result = divAst (Val 123) (Val 456)
      in assertEqual "divAst failed to parse partially matching input" (Just (Val 0)) result,
    testCase "divAst with multiple matching inputs" $
      let result = divAst (Val 123) (Val 456)
      in assertEqual "divAst failed to parse multiple matching inputs" (Just (Val 0)) result
  ]

testModAst :: TestTree
testModAst = testGroup "modAst tests"
  [
    testCase "modAst with matching input" $
      let result = modAst (Val 123) (Val 456)
      in assertEqual "modAst failed to parse matching input" (Just (Val 123)) result,
    testCase "modAst with non-matching input" $
      let result = modAst (Val 123) (Ref "abc")
      in assertEqual "modAst parsed non-matching input" Nothing result,
    testCase "modAst with empty input" $
      let result = modAst (Val 123) (Val 456)
      in assertEqual "modAst failed to parse empty input" (Just (Val 123)) result,
    testCase "modAst with partially matching input" $
      let result = modAst (Val 123) (Val 456)
      in assertEqual "modAst failed to parse partially matching input" (Just (Val 123)) result,
    testCase "modAst with multiple matching inputs" $
      let result = modAst (Val 123) (Val 456)
      in assertEqual "modAst failed to parse multiple matching inputs" (Just (Val 123)) result
  ]

testEqAst :: TestTree
testEqAst = testGroup "eqAst tests"
  [
    testCase "eqAst with matching input" $
      let result = eqAst (Val 123) (Val 456)
      in assertEqual "eqAst failed to parse matching input" (Just (Boolean False)) result,
    testCase "eqAst with non-matching input" $
      let result = eqAst (Val 123) (Ref "abc")
      in assertEqual "eqAst parsed non-matching input" Nothing result,
    testCase "eqAst with empty input" $
      let result = eqAst (Val 123) (Val 456)
      in assertEqual "eqAst failed to parse empty input" (Just (Boolean False)) result,
    testCase "eqAst with partially matching input" $
      let result = eqAst (Val 123) (Val 456)
      in assertEqual "eqAst failed to parse partially matching input" (Just (Boolean False)) result,
    testCase "eqAst with multiple matching inputs" $
      let result = eqAst (Val 123) (Val 456)
      in assertEqual "eqAst failed to parse multiple matching inputs" (Just (Boolean False)) result
  ]

testLowerAst :: TestTree
testLowerAst = testGroup "lowerAst tests"
  [
    testCase "lowerAst with matching input" $
      let result = lowerAst (Val 123) (Val 456)
      in assertEqual "lowerAst failed to parse matching input" (Just (Boolean True)) result,
    testCase "lowerAst with non-matching input" $
      let result = lowerAst (Val 123) (Ref "abc")
      in assertEqual "lowerAst parsed non-matching input" Nothing result,
    testCase "lowerAst with empty input" $
      let result = lowerAst (Val 123) (Val 456)
      in assertEqual "lowerAst failed to parse empty input" (Just (Boolean True)) result,
    testCase "lowerAst with partially matching input" $
      let result = lowerAst (Val 123) (Val 456)
      in assertEqual "lowerAst failed to parse partially matching input" (Just (Boolean True)) result,
    testCase "lowerAst with multiple matching inputs" $
      let result = lowerAst (Val 123) (Val 456)
      in assertEqual "lowerAst failed to parse multiple matching inputs" (Just (Boolean True)) result
  ]

testExtractLambdaFromSet :: TestTree
testExtractLambdaFromSet = testGroup "extractLambdaFromSet tests"
  [
    testCase "extractLambdaFromSet with matching input" $
      let result = extractLambdaFromSet (Ref "abc") (LambdaSet [Lambda (Ref "abc") [] (Val 123)])
      in assertEqual "extractLambdaFromSet failed to parse matching input" (Just (Lambda (Ref "abc") [] (Val 123))) result,
    testCase "extractLambdaFromSet with non-matching input" $
      let result = extractLambdaFromSet (Ref "abc") (LambdaSet [Lambda (Ref "def") [] (Val 123)])
      in assertEqual "extractLambdaFromSet parsed non-matching input" Nothing result,
    testCase "extractLambdaFromSet with empty input" $
      let result = extractLambdaFromSet (Ref "abc") (LambdaSet [])
      in assertEqual "extractLambdaFromSet failed to parse empty input" Nothing result,
    testCase "extractLambdaFromSet with partially matching input" $
      let result = extractLambdaFromSet (Ref "abc") (LambdaSet [Lambda (Ref "abc") [] (Val 123), Lambda (Ref "def") [] (Val 456)])
      in assertEqual "extractLambdaFromSet failed to parse partially matching input" (Just (Lambda (Ref "abc") [] (Val 123))) result,
    testCase "extractLambdaFromSet with multiple matching inputs" $
      let result = extractLambdaFromSet (Ref "abc") (LambdaSet [Lambda (Ref "abc") [] (Val 123), Lambda (Ref "abc") [] (Val 456)])
      in assertEqual "extractLambdaFromSet failed to parse multiple matching inputs" (Just (Lambda (Ref "abc") [] (Val 123))) result
  ]

testRefListToStringList :: TestTree
testRefListToStringList = testGroup "refListToStringList tests"
  [
    testCase "refListToStringList with matching input" $
      let result = refListToStringList [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "refListToStringList failed to parse matching input" ["abc", "def", "ghi"] result,
    testCase "refListToStringList with non-matching input" $
      let result = refListToStringList [Ref "abc", Val 123, Ref "ghi"]
      in assertEqual "refListToStringList parsed non-matching input" ["abc", "ghi"] result,
    testCase "refListToStringList with empty input" $
      let result = refListToStringList []
      in assertEqual "refListToStringList failed to parse empty input" [] result,
    testCase "refListToStringList with partially matching input" $
      let result = refListToStringList [Ref "abc", Val 123, Ref "ghi"]
      in assertEqual "refListToStringList failed to parse partially matching input" ["abc", "ghi"] result,
    testCase "refListToStringList with multiple matching inputs" $
      let result = refListToStringList [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "refListToStringList failed to parse multiple matching inputs" ["abc", "def", "ghi"] result
  ]

testAstElemIndex :: TestTree
testAstElemIndex = testGroup "astElemIndex tests"
  [
    testCase "astElemIndex with matching input" $
      let result = astElemIndex 0 (Ref "abc") [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "astElemIndex failed to parse matching input" (Just 0) result,
    testCase "astElemIndex with non-matching input" $
      let result = astElemIndex 0 (Ref "abc") [Ref "def", Ref "ghi"]
      in assertEqual "astElemIndex parsed non-matching input" Nothing result,
    testCase "astElemIndex with empty input" $
      let result = astElemIndex 0 (Ref "abc") []
      in assertEqual "astElemIndex failed to parse empty input" Nothing result,
    testCase "astElemIndex with partially matching input" $
      let result = astElemIndex 0 (Ref "abc") [Ref "def", Ref "ghi"]
      in assertEqual "astElemIndex failed to parse partially matching input" Nothing result,
    testCase "astElemIndex with multiple matching inputs" $
      let result = astElemIndex 0 (Ref "abc") [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "astElemIndex failed to parse multiple matching inputs" (Just 0) result
  ]

testGetAstAtIndex :: TestTree
testGetAstAtIndex = testGroup "getAstAtIndex tests"
  [
    testCase "getAstAtIndex with matching input" $
      let result = getAstAtIndex 0 [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "getAstAtIndex failed to parse matching input" (Just (Ref "abc")) result,
    testCase "getAstAtIndex with non-matching input" $
      let result = getAstAtIndex 0 [Ref "def", Ref "ghi"]
      in assertEqual "getAstAtIndex parsed non-matching input" (Just (Ref "def")) result,
    testCase "getAstAtIndex with empty input" $
      let result = getAstAtIndex 0 []
      in assertEqual "getAstAtIndex failed to parse empty input" Nothing result,
    testCase "getAstAtIndex with partially matching input" $
      let result = getAstAtIndex 0 [Ref "def", Ref "ghi"]
      in assertEqual "getAstAtIndex failed to parse partially matching input" (Just (Ref "def")) result,
    testCase "getAstAtIndex with multiple matching inputs" $
      let result = getAstAtIndex 0 [Ref "abc", Ref "def", Ref "ghi"]
      in assertEqual "getAstAtIndex failed to parse multiple matching inputs" (Just (Ref "abc")) result
  ]

testGetArgsForCall :: TestTree
testGetArgsForCall = testGroup "getArgsForCall tests"
  [
    testCase "getArgsForCall with matching input" $
      let result = getArgsForCall [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] (Ref "abc")
      in assertEqual "getArgsForCall failed to parse matching input" (Just (Ref "jkl")) result,
    testCase "getArgsForCall with non-matching input" $
      let result = getArgsForCall [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] (Ref "xyz")
      in assertEqual "getArgsForCall parsed non-matching input" Nothing result,
    testCase "getArgsForCall with empty input" $
      let result = getArgsForCall [] [] (Ref "abc")
      in assertEqual "getArgsForCall failed to parse empty input" Nothing result,
    testCase "getArgsForCall with partially matching input" $
      let result = getArgsForCall [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] (Ref "xyz")
      in assertEqual "getArgsForCall failed to parse partially matching input" Nothing result,
    testCase "getArgsForCall with multiple matching inputs" $
      let result = getArgsForCall [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] (Ref "abc")
      in assertEqual "getArgsForCall failed to parse multiple matching inputs" (Just (Ref "jkl")) result
  ]

testGetArgsForCall2 :: TestTree
testGetArgsForCall2 = testGroup "getArgsForCall2 tests"
  [
    testCase "getArgsForCall2 with matching input" $
      let result = getArgsForCall2 [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] [Ref "abc"]
      in assertEqual "getArgsForCall2 failed to parse matching input" [Ref "jkl"] result,
    testCase "getArgsForCall2 with non-matching input" $
      let result = getArgsForCall2 [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] [Ref "xyz"]
      in assertEqual "getArgsForCall2 parsed non-matching input" [] result,
    testCase "getArgsForCall2 with empty input" $
      let result = getArgsForCall2 [] [] [Ref "abc"]
      in assertEqual "getArgsForCall2 failed to parse empty input" [] result,
    testCase "getArgsForCall2 with partially matching input" $
      let result = getArgsForCall2 [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] [Ref "xyz"]
      in assertEqual "getArgsForCall2 failed to parse partially matching input" [] result,
    testCase "getArgsForCall2 with multiple matching inputs" $
      let result = getArgsForCall2 [Ref "abc", Ref "def", Ref "ghi"] [Ref "jkl", Ref "mno", Ref "pqr"] [Ref "abc"]
      in assertEqual "getArgsForCall2 failed to parse multiple matching inputs" [Ref "jkl"] result
  ]