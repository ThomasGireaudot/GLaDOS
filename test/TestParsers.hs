module TestParsers where

import SharedFunc
import Test.Tasty
import Test.Tasty.HUnit
import Running
import Datatypes
import Parsers
import Data.Maybe (fromJust, fromMaybe)

testParseChar :: TestTree
testParseChar = testGroup "test parsechar"
    [
        testCase "parsechar with matching char" $
            let (Parser p) = parseChar 'a'
            in do
                -- Vérifie que parseChar renvoie une valeur Just ('a', _) pour "abc"
                let result = p "abc"
                assertBool "parseChar 'a' failed to parse matching char" $ isJust result
                -- Vérifie que le reste du texte est "bc"
                let (_, rest) = fromJust result
                assertEqual "parseChar 'a' failed to consume rest of input" "bc" rest,
        testCase "parsechar with non-matching char" $
            let (Parser p) = parseChar 'a'
            in do
                -- Vérifie que parseChar renvoie une valeur Nothing pour "bc"
                let result = p "bc"
                assertBool "parseChar 'a' parsed non-matching char" $ isNothing result
                -- Vérifie que le reste du texte est "bc"
                let (_, rest) = fromMaybe (' ', "") result
                assertEqual "parseChar 'a' consumed input despite non-matching char" "" rest,
        testCase "parsechar with empty input" $
            let (Parser p) = parseChar 'a'
            in do
                -- Vérifie que parseChar renvoie une valeur Nothing pour ""
                let result = p ""
                assertBool "parseChar 'a' parsed empty input" $ isNothing result
                -- Vérifie que le reste du texte est ""
                let (_, rest) = fromMaybe (' ', "") result
                assertEqual "parseChar 'a' consumed input despite empty input" "" rest
    ]


testParseAnyChar :: TestTree
testParseAnyChar = testGroup "test testanychar"
    [
        testCase "testanychar with matching char" $
            let (Parser p) = parseAnyChar "abc"
            in do
            -- Vérifie que parseAnyChar renvoie une valeur Just ('a', _) pour "abc"
            let result = p "abc"
            assertBool "parseAnyChar 'a' failed to parse matching char" $ isJust result
            -- Vérifie que le reste du texte est "bc"
            let (_, rest) = fromJust result
            assertEqual "parseAnyChar 'a' failed to consume rest of input" "bc" rest,
        testCase "parseAnyChar with non-matching char" $
            let (Parser p) = parseAnyChar "abc"
            in do
                let result = p "d"
                assertBool "parseAnyChar 'd' should return Nothing" $ isNothing result,
        testCase "parseAnyChar with empty input" $
            let (Parser p) = parseAnyChar "abc"
            in do
                -- Vérifie que parseAnyChar renvoie une valeur Nothing pour ""
                let result = p ""
                assertBool "parseAnyChar 'a' parsed empty input" $ isNothing result
    ]

testParseOr :: TestTree
testParseOr = testGroup "test parseor"
    [
        testCase "testparseor with matching char" $
            let (Parser p) = parseOr (parseChar 'a') (parseChar 'b')
            in do
                -- Vérifie que parseOr renvoie une valeur Just ('a', _) pour "abc"
                let result = p "abc"
                assertBool "parseOr 'a' failed to parse matching char" $ isJust result
                -- Vérifie que le reste du texte est "bc"
                let (_, rest) = fromJust result
                assertEqual "parseOr 'a' failed to consume rest of input" "bc" rest
        , testCase "parseOr with non-matching char" $
            let (Parser p) = parseOr (parseChar 'a') (parseChar 'b')
            in do
                -- Vérifie que parseOr renvoie une valeur Nothing pour "bc"
                let result = p "c"
                assertBool "parseOr 'a' parsed non-matching char" $ isNothing result
        , testCase "parseOr with empty input" $
            let (Parser p) = parseOr (parseChar 'a') (parseChar 'b')
            in do
                -- Vérifie que parseOr renvoie une valeur Nothing pour ""
                let result = p ""
                assertBool "parseOr 'a' parsed empty input" $ isNothing result
    ]

testParseAnd :: TestTree
testParseAnd = testGroup "parseAnd tests"
  [
    testCase "parseAnd with matching input" $
      let (Parser p) = parseAnd (parseChar 'a') (parseChar 'b')
          result = p "ab"
      in assertEqual "parseAnd failed to parse matching input" (Just ('a','b')) (fst <$> result)
  , testCase "parseAnd with non-matching input" $
      let (Parser p) = parseAnd (parseChar 'a') (parseChar 'b')
          result = p "ac"
      in assertEqual "parseAnd parsed non-matching input" Nothing result
  , testCase "parseAnd with empty input" $
      let (Parser p) = parseAnd (parseChar 'a') (parseChar 'b')
          result = p ""
      in assertEqual "parseAnd parsed empty input" Nothing result
  ]

testParseMany :: TestTree
testParseMany = testGroup "parseMany tests"
  [
    testCase "parseMany with matching input" $
      let (Parser p) = parseMany (parseChar 'a')
          result = p "aaa"
      in assertEqual "parseMany failed to parse matching input" (Just ("aaa", "")) result,
    testCase "parseMany with non-matching input" $
      let (Parser p) = parseMany (parseChar 'a')
          result = p "ab"
      in assertEqual "parseMany parsed non-matching input" (Just "b") (snd <$> result)

  , testCase "parseMany with empty input" $
      let (Parser p) = parseMany (parseChar 'a')
          result = p ""
      in assertEqual "parseMany parsed empty input" (Just "") (snd <$> result)
  ]

testParseSome :: TestTree
testParseSome = testGroup "parseSome tests"
  [
    testCase "parseSome with matching input" $
      let (Parser p) = parseSome (parseChar 'a')
          result = p "aaa"
      in assertEqual "parseSome failed to parse matching input" (Just ("aaa", "")) result,
    testCase "parseSome with non-matching input" $
      let (Parser p) = parseSome (parseChar 'c')
          result = p "ab"
      in assertEqual "parseSome parsed non-matching input" Nothing result,
    testCase "parseSome with empty input" $
      let (Parser p) = parseSome (parseChar 'a')
          result = p ""
      in assertEqual "parseSome failed to parse empty input" Nothing result,
    testCase "parseSome with partially matching input" $
      let (Parser p) = parseSome (parseChar 'a')
          result = p "aab"
      in assertEqual "parseSome failed to parse partially matching input" (Just ("aa", "b")) result,
    testCase "parseSome with multiple matching inputs" $
      let (Parser p) = parseSome (parseChar 'a')
          result = p "aaab"
      in assertEqual "parseSome failed to parse multiple matching inputs" (Just ("aaa", "b")) result
  ]

testParseUInt :: TestTree
testParseUInt = testGroup "parseUInt tests"
  [
    testCase "parseUInt with matching input" $
      let (Parser p) = parseUInt
          result = p "123"
      in assertEqual "parseUInt failed to parse matching input" (Just (123, "")) result,
    testCase "parseUInt with non-matching input" $
      let (Parser p) = parseUInt
          result = p "abc"
      in assertEqual "parseUInt parsed non-matching input" Nothing result,
    testCase "parseUInt with empty input" $
      let (Parser p) = parseUInt
          result = p ""
      in assertEqual "parseUInt failed to parse empty input" Nothing result,
    testCase "parseUInt with partially matching input" $
      let (Parser p) = parseUInt
          result = p "123abc"
      in assertEqual "parseUInt failed to parse partially matching input" (Just (123, "abc")) result,
    testCase "parseUInt with multiple matching inputs" $
      let (Parser p) = parseUInt
          result = p "123abc456"
      in assertEqual "parseUInt failed to parse multiple matching inputs" (Just (123, "abc456")) result
  ]

testParseInt :: TestTree
testParseInt = testGroup "parseInt tests"
  [
    testCase "parseInt with matching input" $
      let (Parser p) = parseInt
          result = p "-123"
      in assertEqual "parseInt failed to parse matching input" (Just (-123, "")) result,
    testCase "parseInt with non-matching input" $
      let (Parser p) = parseInt
          result = p "abc"
      in assertEqual "parseInt parsed non-matching input" Nothing result,
    testCase "parseInt with empty input" $
      let (Parser p) = parseInt
          result = p ""
      in assertEqual "parseInt failed to parse empty input" Nothing result,
    testCase "parseInt with partially matching input" $
      let (Parser p) = parseInt
          result = p "-123abc"
      in assertEqual "parseInt failed to parse partially matching input" (Just (-123, "abc")) result,
    testCase "parseInt with multiple matching inputs" $
      let (Parser p) = parseInt
          result = p "-123abc456"
      in assertEqual "parseInt failed to parse multiple matching inputs" (Just (-123, "abc456")) result
  ]

testParsePair :: TestTree
testParsePair = testGroup "parsePair tests"
  [
    testCase "parsePair with matching input" $
      let (Parser p) = parsePair parseUInt
          result = p "( 123 456 )"
      in assertEqual "parsePair failed to parse matching input" (Just ((123,456), "")) result,
    testCase "parsePair with non-matching input" $
      let (Parser p) = parsePair parseUInt
          result = p "( 123 abc )"
      in assertEqual "parsePair parsed non-matching input" Nothing result,
    testCase "parsePair with empty input" $
      let (Parser p) = parsePair parseUInt
          result = p ""
      in assertEqual "parsePair failed to parse empty input" Nothing result,
    testCase "parsePair with partially matching input" $
      let (Parser p) = parsePair parseUInt
          result = p "( 123 456 abc )"
      in assertEqual "parsePair failed to parse partially matching input" Nothing result,
    testCase "parsePair with multiple matching inputs" $
      let (Parser p) = parsePair parseUInt
          result = p "( 123 456 ) ( 789 101112 )"
      in assertEqual "parsePair failed to parse multiple matching inputs" (Just ((123,456), " ( 789 101112 )")) result
  ]

testParseCpt :: TestTree
testParseCpt = testGroup "parseCpt tests"
  [
    testCase "parseCpt with matching input" $
      let (Parser p) = parseCpt
          result = p "123"
      in assertEqual "parseCpt failed to parse matching input" (Just (Nbr 123, "")) result,
    testCase "parseCpt with non-matching input" $
      let (Parser p) = parseCpt
          result = p "abc"
      in assertEqual "parseCpt parsed non-matching input" Nothing result,
    testCase "parseCpt with empty input" $
      let (Parser p) = parseCpt
          result = p ""
      in assertEqual "parseCpt failed to parse empty input" Nothing result,
    testCase "parseCpt with partially matching input" $
      let (Parser p) = parseCpt
          result = p "123abc"
      in assertEqual "parseCpt failed to parse partially matching input" (Just (Nbr 123, "abc")) result,
    testCase "parseCpt with multiple matching inputs" $
      let (Parser p) = parseCpt
          result = p "123abc456"
      in assertEqual "parseCpt failed to parse multiple matching inputs" (Just (Nbr 123, "abc456")) result
  ]


testParseNbr :: TestTree
testParseNbr = testGroup "parseNbr tests"
  [
    testCase "parseNbr with matching input" $
      let (Parser p) = parseNbr
          result = p "123"
      in assertEqual "parseNbr failed to parse matching input" (Just (Nbr 123, "")) result,
    testCase "parseNbr with non-matching input" $
      let (Parser p) = parseNbr
          result = p "abc"
      in assertEqual "parseNbr parsed non-matching input" Nothing result,
    testCase "parseNbr with empty input" $
      let (Parser p) = parseNbr
          result = p ""
      in assertEqual "parseNbr failed to parse empty input" Nothing result,
    testCase "parseNbr with partially matching input" $
      let (Parser p) = parseNbr
          result = p "123abc"
      in assertEqual "parseNbr failed to parse partially matching input" (Just (Nbr 123, "abc")) result,
    testCase "parseNbr with multiple matching inputs" $
      let (Parser p) = parseNbr
          result = p "123abc456"
      in assertEqual "parseNbr failed to parse multiple matching inputs" (Just (Nbr 123, "abc456")) result
  ]

testParseSymbol :: TestTree
testParseSymbol = testGroup "parseSymbol tests"
  [
    testCase "parseSymbol with matching input" $
      let (Parser p) = parseSymbol
          result = p "abc"
      in assertEqual "parseSymbol failed to parse matching input" (Just (Symbol "abc", "")) result,
    testCase "parseSymbol with non-matching input" $
      let (Parser p) = parseSymbol
          result = p "§£~"
      in assertEqual "parseSymbol parsed non-matching input" Nothing result,
    testCase "parseSymbol with empty input" $
      let (Parser p) = parseSymbol
          result = p ""
      in assertEqual "parseSymbol failed to parse empty input" Nothing result,
    testCase "parseSymbol with partially matching input" $
      let (Parser p) = parseSymbol
          result = p "abc§£~"
      in assertEqual "parseSymbol failed to parse partially matching input" (Just (Symbol "abc", "§£~")) result,
    testCase "parseSymbol with multiple matching inputs" $
      let (Parser p) = parseSymbol
          result = p "abc123def"
      in assertEqual "parseSymbol failed to parse multiple matching inputs" (Just (Symbol "abc123def", "")) result
  ]

testParseList :: TestTree
testParseList = testGroup "parseList tests"
  [
    testCase "parseList with matching input" $
      let (Parser p) = parseList
          result = p "( 123 456 )"
      in assertEqual "parseList failed to parse matching input" (Just (List [Nbr 123, Nbr 456], "")) result,
    testCase "parseList with non-matching input" $
      let (Parser p) = parseList
          result = p "( £§~ £§~ )"
      in assertEqual "parseList parsed non-matching input" Nothing result,
    testCase "parseList with empty input" $
      let (Parser p) = parseList
          result = p ""
      in assertEqual "parseList failed to parse empty input" Nothing result,
    testCase "parseList with partially matching input" $
      let (Parser p) = parseList
          result = p "( 123 £§~ abc )"
      in assertEqual "parseList failed to parse partially matching input" Nothing result,
    testCase "parseList with multiple matching inputs" $
      let (Parser p) = parseList
          result = p "( 123 456 ) ( 789 101112 )"
      in assertEqual "parseList failed to parse multiple matching inputs" (Just (List [Nbr 123, Nbr 456], " ( 789 101112 )")) result
  ]