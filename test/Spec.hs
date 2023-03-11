import Test.Tasty
import Test.Tasty.HUnit
import TestRunning
import TestParsers
import TestCPT
import TestAST

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [
    testValOfRefB,
    testValOfRefI,
    testIsInListI,
    testIsInListB,
    testRemoveRefI,
    testRemoveRefB,
    testAstToRef,
    testLambdaExist,
    testAstToLambdas,
    testParseChar,
    testParseAnyChar,
    testParseOr,
    testParseAnd,
    testParseMany,
    testParseSome,
    testParseUInt,
    testParseInt,
    testParsePair,
    testParseNbr,
    testParseSymbol,
    testParseList,
    testPrintTreeList,
    testPrintTree,
    testPrintParsedTree,
    testPrintTreeList,
    testPrintTree,
    testCptToAST,
    testAddAst,
    testSubAst,
    testMulAst,
    testDivAst,
    testModAst,
    testEqAst,
    testLowerAst,
    testExtractLambdaFromSet,
    testRefListToStringList,
    testAstElemIndex,
    testGetAstAtIndex,
    testGetArgsForCall,
    testGetArgsForCall2
  ]
