module TestDatatypes where

import SharedFunc
import Test.Tasty
import Test.Tasty.HUnit
import Datatypes

testCreateReferenceSet :: TestTree
testCreateReferenceSet = testGroup "testCreateReferenceSet"
    [
        testCase "run function" $
            let result = createReferenceSet
            in assertEqual "reference set is not empty" (ReferenceSet [] []) result
    ]

testCreateLambdaSet :: TestTree
testCreateLambdaSet = testGroup "testCreateLambdaSet"
    [
        testCase "run function" $
            let result = createLambdaSet
            in assertEqual "lambda set is not empty" (LambdaSet []) result
    ]