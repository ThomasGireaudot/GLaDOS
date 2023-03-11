{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CPTManipulation where

import Datatypes (Cpt (..))

printTreeList :: [Cpt] -> String
printTreeList (x : y : xs) = printTree x ++ ", " ++ printTreeList (y : xs)
printTreeList (x : _) = printTree x
printTreeList _ = ""

printTree :: Cpt -> String
printTree (Nbr i) = "a number " ++ show i
printTree (Symbol s) = "a symbol " ++ show s
printTree (List (x : y : xs)) = "(a list with " ++ printTree x ++ " followed by " ++ printTreeList (y : xs) ++ ")"
printTree (List l) = "(a list with " ++ printTreeList l ++ ")"

printParsedTree :: Maybe (Cpt, String) -> String
printParsedTree Nothing = ""
printParsedTree (Just (Nbr i, _)) = "a number " ++ show i
printParsedTree (Just (Symbol s, _)) = "a symbol " ++ show s
printParsedTree (Just (List [], _)) = "an empty list"
printParsedTree (Just (List (x : y : xs), _)) = "(a list with " ++ printTree x ++ " followed by " ++ printTreeList (y : xs) ++ ")"
printParsedTree (Just (List l, _)) = "(a list with " ++ printTreeList l ++ ")"
