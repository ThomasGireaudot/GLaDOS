module TestRunning where

import SharedFunc
import Test.Tasty
import Test.Tasty.HUnit
import Datatypes
import Running
import Parsers

testValOfRefI :: TestTree
testValOfRefI = testGroup "Tests valOfRefI"
  [
    testCase "empty list" $ Nothing @=? valOfRefI "key1" [],
    testCase "Not in list" $ Nothing @=? valOfRefI "key1" [(ReferenceInt "key2" 1), (ReferenceInt "key3" 2)],
    testCase "In list" $ Just 1 @=? valOfRefI "key1" [(ReferenceInt "key1" 1), (ReferenceInt "key2" 2)],
    testCase "multiple keys" $ Just 2 @=? valOfRefI "key2" [(ReferenceInt "key1" 1), (ReferenceInt "key2" 2)]
  ]

testValOfRefB :: TestTree
testValOfRefB = testGroup "Tests valOfRefB"
  [
    testCase "empty list" $ Nothing @=? valOfRefB "key1" [],
    testCase "Not in list" $ Nothing @=? valOfRefB "key1" [(ReferenceBool "key2" True), (ReferenceBool "key3" False)],
    testCase "In list" $ Just True @=? valOfRefB "key1" [(ReferenceBool "key1" True), (ReferenceBool "key2" False)],
    testCase "multiple keys" $ Just False @=? valOfRefB "key2" [(ReferenceBool "key1" True), (ReferenceBool "key2" False)]
  ]

testIsInListI :: TestTree
testIsInListI = testGroup "Tests isInListI"
  [
    testCase "Found in list" $ isInListI "a" [(ReferenceInt "a" 1), (ReferenceInt "b" 2), (ReferenceInt "c" 3)] @?= True,
    testCase "Not found in list" $ isInListI "d" [(ReferenceInt "a" 1), (ReferenceInt "b" 2), (ReferenceInt "c" 3)] @?= False,
    testCase "Empty list" $ isInListI "a" [] @?= False
  ]

testIsInListB :: TestTree
testIsInListB = testGroup "Tests isInListB"
  [
    testCase "Found in list" $ isInListB "a" [(ReferenceBool "a" True), (ReferenceBool "b" False), (ReferenceBool "c" True)] @?= True,
    testCase "Not found in list" $ isInListB "d" [(ReferenceBool "a" True), (ReferenceBool "b" False), (ReferenceBool "c" True)] @?= False,
    testCase "Empty list" $ isInListB "a" [] @?= False
  ]

testRemoveRefI :: TestTree
testRemoveRefI = testGroup "removeRefI Tests"
  [ testCase "Remove Reference Int Test 1" $ removeRefI "name1" [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2), (ReferenceInt "name3" 3)] @?= [(ReferenceInt "name2" 2), (ReferenceInt "name3" 3)],
    testCase "Remove Reference Int Test 2" $ removeRefI "name2" [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2), (ReferenceInt "name3" 3)] @?= [(ReferenceInt "name1" 1), (ReferenceInt "name3" 3)],
    testCase "Remove Reference Int Test 3" $ removeRefI "name3" [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2), (ReferenceInt "name3" 3)] @?= [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2)],
    testCase "Remove Reference Int Test 4" $ removeRefI "name4" [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2), (ReferenceInt "name3" 3)] @?= [(ReferenceInt "name1" 1), (ReferenceInt "name2" 2), (ReferenceInt "name3" 3)]
  ]

testRemoveRefB :: TestTree
testRemoveRefB = testGroup "removeRefB Tests"
  [ testCase "Remove Reference Bool Test 1" $ removeRefB "name1" [(ReferenceBool "name1" True), (ReferenceBool "name2" False), (ReferenceBool "name3" True)] @?= [(ReferenceBool "name2" False), (ReferenceBool "name3" True)],
    testCase "Remove Reference Bool Test 2" $ removeRefB "name2" [(ReferenceBool "name1" True), (ReferenceBool "name2" False), (ReferenceBool "name3" True)] @?= [(ReferenceBool "name1" True), (ReferenceBool "name3" True)],
    testCase "Remove Reference Bool Test 3" $ removeRefB "name3" [(ReferenceBool "name1" True), (ReferenceBool "name2" False), (ReferenceBool "name3" True)] @?= [(ReferenceBool "name1" True), (ReferenceBool "name2" False)],
    testCase "Remove Reference Bool Test 4" $ removeRefB "name4" [(ReferenceBool "name1" True), (ReferenceBool "name2" False), (ReferenceBool "name3" True)] @?= [(ReferenceBool "name1" True), (ReferenceBool "name2" False), (ReferenceBool "name3" True)]
  ]

testAstToRef :: TestTree
testAstToRef = testGroup "AstToRef tests" [testAstToRefDefineInt, testAstToRefDefineBool, testAstToRefDefineRef, testAstToRefDefineCall]

testAstToRefDefineInt :: TestTree
testAstToRefDefineInt = testCase "AstToRef on Define with Val" $ do
    let ast = Define "name" (Val 3)
    let refSet = ReferenceSet [] []
    let lset = LambdaSet []
    let expected = Just (ReferenceSet [ReferenceInt "name" 3] [])
    assertEqual "Define Val result not as expected" expected (astToRef ast refSet lset)

testAstToRefDefineBool :: TestTree
testAstToRefDefineBool = testCase "AstToRef on Define with Boolean" $ do
    let ast = Define "name" (Boolean True)
    let refSet = ReferenceSet [] []
    let lset = LambdaSet []
    let expected = Just (ReferenceSet [] [ReferenceBool "name" True])
    assertEqual "Define Boolean result not as expected" expected (astToRef ast refSet lset)

testAstToRefDefineRef :: TestTree
testAstToRefDefineRef = testCase "AstToRef on Define with Ref" $ do
    let ast = Define "name" (Ref "s")
    let refSet = ReferenceSet [ReferenceInt "s" 3] []
    let lset = LambdaSet []
    let expected = Just (ReferenceSet [ReferenceInt "name" 3, ReferenceInt "s" 3] [])
    assertEqual "Define Ref result not as expected" expected (astToRef ast refSet lset)

testAstToRefDefineCall :: TestTree
testAstToRefDefineCall = testCase "AstToRef on Define with Call" $ do
    let ast = Define "name" (Call (Ref "+") [Val 3, Val 4])
    let refSet = ReferenceSet [] []
    let lset = LambdaSet []
    let expected = Just (ReferenceSet [ReferenceInt "name" 7] [])
    assertEqual "Define Call result not as expected" expected (astToRef ast refSet lset)

testLambdaExist :: TestTree
testLambdaExist = testGroup "LambdaExist test"
  [
    testCase "Test 1" $ lambdaExist "lambda" [Lambda (Ref "lambda") [Ref "arg1"] (Val 5)] @?= True,
    testCase "Test 2" $ lambdaExist "lambda1" [Lambda (Ref "lambda") [Ref "arg1"] (Val 5), Lambda (Ref "lambda1") [Ref "arg1", Ref "arg2"] (Boolean False)] @?= True,
    testCase "Test 3" $ lambdaExist "lambda2" [Lambda (Ref "lambda") [Ref "arg1"] (Val 5), Lambda (Ref "lambda1") [Ref "arg1", Ref "arg2"] (Boolean False)] @?= False,
    testCase "Test 4" $ lambdaExist "lambda2" [] @?= False
  ]

testAstToLambdas :: TestTree
testAstToLambdas = testGroup "astToLambdas tests"
  [
    testCase "lambda does not exist in set" $ astToLambdas (Lambda (Ref "f") [Ref "x"] (Ref "x")) (LambdaSet []) @?= Just (LambdaSet [Lambda (Ref "f") [Ref "x"] (Ref "x")]),
    testCase "lambda exists in set" $ astToLambdas (Lambda (Ref "f") [Ref "y"] (Ref "y")) (LambdaSet [Lambda (Ref "f") [Ref "x"] (Ref "x")]) @?= Just (LambdaSet [Lambda (Ref "f") [Ref "y"] (Ref "y")]),
    testCase "other Ast" $ astToLambdas (Ref "x") (LambdaSet []) @?= Nothing
  ]
