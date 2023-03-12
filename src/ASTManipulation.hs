{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module ASTManipulation where

import Datatypes (Ast (..), Cpt (..), LambdaSet (..))
import GHC.Base (eqString)

cptToAST :: Cpt -> Maybe Ast
cptToAST (Symbol ['#', 't']) = Just (Boolean True)
cptToAST (Symbol ['#', 'f']) = Just (Boolean False)
cptToAST (Symbol s) = Just (Ref s)
cptToAST (Nbr n) = Just (Val n)
cptToAST (List []) = Nothing
cptToAST (List [Symbol x, Symbol "=", val]) = cptToAST val >>= \ast -> Just (Define x ast)
cptToAST (List [Symbol "define", Symbol name, List [], List body]) =
  cptToAST (Symbol name) >>= \name2 ->
    cptToAST (List body) >>= \ast -> Just (Lambda name2 [] ast)
cptToAST (List [Symbol "define", Symbol name, List args, List body]) =
  cptToAST (Symbol name) >>= \name2 ->
    mapM cptToAST args >>= \params ->
      cptToAST (List body) >>= \ast -> Just (Lambda name2 params ast)
cptToAST (List [Symbol "if", c, t, e]) =
  cptToAST c >>= \c' ->
    cptToAST t >>= \t' ->
      cptToAST e >>= \e' ->
        Just (Condition c' t' e')
cptToAST (List [Symbol "while", c, b]) =
  cptToAST c >>= \c' ->
    cptToAST b >>= \b' ->
      Just (Loop c' b')
cptToAST (List [x, Symbol sign, y]) =
  cptToAST x >>= \astX ->
    cptToAST y >>= \astY -> case sign of
      "==" -> Just (Call (Ref "eq?") [astX, astY])
      "+" -> Just (Call (Ref "+") [astX, astY])
      "-" -> Just (Call (Ref "-") [astX, astY])
      "*" -> Just (Call (Ref "*") [astX, astY])
      "/" -> Just (Call (Ref "div") [astX, astY])
      "%" -> Just (Call (Ref "mod") [astX, astY])
      _ -> Nothing
cptToAST (List (cname : cargs)) =
  cptToAST cname >>= \name ->
    mapM cptToAST cargs >>= \args ->
      Just (Call name args)

addAst :: Ast -> Ast -> Maybe Ast
addAst (Val a) (Val b) = Just (Val (a + b))
addAst _ _ = Nothing

subAst :: Ast -> Ast -> Maybe Ast
subAst (Val a) (Val b) = Just (Val (a - b))
subAst _ _ = Nothing

mulAst :: Ast -> Ast -> Maybe Ast
mulAst (Val a) (Val b) = Just (Val (a * b))
mulAst _ _ = Nothing

divAst :: Ast -> Ast -> Maybe Ast
divAst _ (Val 0) = Nothing
divAst (Val a) (Val b) = Just (Val (a `div` b))
divAst _ _ = Nothing

modAst :: Ast -> Ast -> Maybe Ast
modAst _ (Val 0) = Nothing
modAst (Val a) (Val b) = Just (Val (a `mod` b))
modAst _ _ = Nothing

eqAst :: Ast -> Ast -> Maybe Ast
eqAst (Val a) (Val b) = Just (Boolean (a == b))
eqAst (Boolean a) (Boolean b) = Just (Boolean (a == b))
eqAst _ _ = Nothing

lowerAst :: Ast -> Ast -> Maybe Ast
lowerAst (Val a) (Val b)
  | a == b = Just (Boolean False)
  | a < b = Just (Boolean True)
  | otherwise = Just (Boolean False)
lowerAst _ _ = Nothing

upperAst :: Ast -> Ast -> Maybe Ast
upperAst (Val a) (Val b)
  | a == b = Just (Boolean False)
  | a > b = Just (Boolean True)
  | otherwise = Just (Boolean False)
upperAst _ _ = Nothing

extractLambdaFromSet :: Ast -> LambdaSet -> Maybe Ast
extractLambdaFromSet _ (LambdaSet []) = Nothing
extractLambdaFromSet (Ref name) (LambdaSet ((Lambda (Ref lname) args procedure) : xs))
  | name == lname = Just (Lambda (Ref lname) args procedure)
  | otherwise = extractLambdaFromSet (Ref name) (LambdaSet xs)
extractLambdaFromSet _ _ = Nothing

refListToStringList :: [Ast] -> [String]
refListToStringList ((Ref str) : xs) = str : refListToStringList xs
refListToStringList (_ : xs) = refListToStringList xs
refListToStringList [] = []

astElemIndex :: Int -> Ast -> [Ast] -> Maybe Int
astElemIndex _ _ [] = Nothing
astElemIndex index (Ref str) (Ref x : xs)
  | str == x = Just index
  | otherwise = astElemIndex (index + 1) (Ref str) xs
astElemIndex _ _ _ = Nothing

getAstAtIndex :: Int -> [Ast] -> Maybe Ast
getAstAtIndex x list
  | null list = Nothing
  | x == 0 = Just (head list)
  | otherwise = getAstAtIndex (x - 1) $ tail list

getArgsForCall :: [Ast] -> [Ast] -> Ast -> Maybe Ast
getArgsForCall _ _ (Boolean x) = Just (Boolean x)
getArgsForCall _ _ (Val x) = Just (Val x)
getArgsForCall params userArgs (Ref x) = case astElemIndex 0 (Ref x) params of
  Just index -> getAstAtIndex index userArgs
  Nothing -> Nothing
getArgsForCall params userArgs (Condition (Call name c) t e) = Just (Condition c' t' e')
  where
    c' = Call name (getArgsForCall2 params userArgs c)
    t' = case t of
      (Call tname targs) -> Call tname (getArgsForCall2 params userArgs targs)
      _ -> head (getArgsForCall2 params userArgs [t])
    e' = case e of
      (Call ename eargs) -> Call ename (getArgsForCall2 params userArgs eargs)
      _ -> head (getArgsForCall2 params userArgs [e])
getArgsForCall _ _ _ = Nothing

-- params -> userArgs -> body
getArgsForCall2 :: [Ast] -> [Ast] -> [Ast] -> [Ast]
getArgsForCall2 _ _ [] = []
getArgsForCall2 params userArgs (Val x : xs) = Val x : getArgsForCall2 params userArgs xs
getArgsForCall2 params userArgs (Ref x : xs) = case astElemIndex 0 (Ref x) params of
  Just index -> case getAstAtIndex index userArgs of
    Just ast -> ast : getArgsForCall2 params userArgs xs
    Nothing -> []
  Nothing -> []
getArgsForCall2 params userArgs (Call name cargs : xs) = Call name (getArgsForCall2 params userArgs cargs) : getArgsForCall2 params userArgs xs
getArgsForCall2 params userArgs ((Condition (Call name c) t e) : xs) = Condition c' t' e' : getArgsForCall2 params userArgs xs
  where
    c' = Call name (getArgsForCall2 params userArgs c)
    t' = case t of
      (Call tname targs) -> Call tname (getArgsForCall2 params userArgs targs)
      _ -> head (getArgsForCall2 params userArgs [t])
    e' = case e of
      (Call ename eargs) -> Call ename (getArgsForCall2 params userArgs eargs)
      _ -> head (getArgsForCall2 params userArgs [e])
getArgsForCall2 _ _ _ = []

evalAST :: Ast -> LambdaSet -> Maybe Ast
evalAST (Val x) _ = Just (Val x)
evalAST (Boolean x) _ = Just (Boolean x)
evalAST (Call (Val x) []) _ = Just (Val x)
evalAST (Call (Boolean x) []) _ = Just (Boolean x)
evalAST (Call (Ref "+") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      addAst a b
evalAST (Call (Ref "-") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      subAst a b
evalAST (Call (Ref "*") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      mulAst a b
evalAST (Call (Ref "div") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      divAst a b
evalAST (Call (Ref "mod") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      modAst a b
evalAST (Call (Ref "eq?") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      eqAst a b
evalAST (Call (Ref "<") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      lowerAst a b
evalAST (Call (Ref ">") [ca, cb]) set =
  evalAST ca set >>= \a ->
    evalAST cb set >>= \b ->
      upperAst a b
evalAST (Call cname cargs) set = case extractLambdaFromSet cname set of
  Just (Lambda _ params (Call cname2 cargs2)) -> evalAST (Call cname2 (getArgsForCall2 params cargs cargs2)) set
  Just (Lambda _ params (Condition c t e)) -> getArgsForCall params cargs (Condition c t e) >>= \cond -> evalAST cond set
  _ -> Nothing
evalAST (Condition c t e) set = case evalAST c set of
  Nothing -> Nothing
  Just (Boolean b) -> if b then evalAST t set else evalAST e set
  _ -> Nothing
evalAST ast _ = Just ast
