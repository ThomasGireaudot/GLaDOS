{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Running where

import ASTManipulation (cptToAST, evalAST)
import Datatypes (Ast (..), Cpt (..), LambdaSet (..), ReferenceBool (..), ReferenceInt (..), ReferenceSet (..))
import GHC.Base (eqString)
import Parsers (Parser (..), parseCptStart, runParser)

inputToCpt :: String -> Maybe (Cpt, String)
inputToCpt = runParser parseCptStart

valOfRefI :: String -> [ReferenceInt] -> Maybe Int
valOfRefI _ [] = Nothing
valOfRefI key ((ReferenceInt name x) : xs)
  | name == key = Just x
  | otherwise = valOfRefI key xs

valOfRefB :: String -> [ReferenceBool] -> Maybe Bool
valOfRefB _ [] = Nothing
valOfRefB key ((ReferenceBool name x) : xs)
  | name == key = Just x
  | otherwise = valOfRefB key xs

isInListI :: String -> [ReferenceInt] -> Bool
isInListI _ [] = False
isInListI name ((ReferenceInt x _) : xs)
  | name == x = True
  | otherwise = isInListI name xs

isInListB :: String -> [ReferenceBool] -> Bool
isInListB _ [] = False
isInListB name ((ReferenceBool x _) : xs)
  | name == x = True
  | otherwise = isInListB name xs

removeRefI :: String -> [ReferenceInt] -> [ReferenceInt]
removeRefI _ [] = []
removeRefI name ((ReferenceInt nI valI) : xs)
  | nI == name = xs
  | otherwise = ReferenceInt nI valI : removeRefI name xs

removeRefB :: String -> [ReferenceBool] -> [ReferenceBool]
removeRefB _ [] = []
removeRefB name ((ReferenceBool nB valB) : xs)
  | nB == name = xs
  | otherwise = ReferenceBool nB valB : removeRefB name xs

astToRef :: Ast -> ReferenceSet -> LambdaSet -> Maybe ReferenceSet
astToRef (Define name (Val x)) (ReferenceSet refI refB) lset =
  case isInListI name refI of
    False -> (if isInListB name refB then Just (ReferenceSet (ReferenceInt name x : refI) (removeRefB name refB)) else Just (ReferenceSet (ReferenceInt name x : refI) refB))
    True ->
      ( if eqString name nI
          then Just (ReferenceSet (ReferenceInt name x : tail refI) refB)
          else
            astToRef (Define name (Val x)) (ReferenceSet xs refB) lset >>= \(ReferenceSet refI2 _) -> Just (ReferenceSet (ReferenceInt nI valI : refI2) refB)
      )
      where
        (ReferenceInt nI valI) = head refI
        xs = tail refI
astToRef (Define name (Boolean x)) (ReferenceSet refI refB) lset =
  case isInListB name refB of
    False -> (if isInListI name refI then Just (ReferenceSet (removeRefI name refI) (ReferenceBool name x : refB)) else Just (ReferenceSet refI (ReferenceBool name x : refB)))
    True ->
      ( if eqString name nB
          then Just (ReferenceSet refI (ReferenceBool name x : tail refB))
          else
            astToRef (Define name (Boolean x)) (ReferenceSet refI xs) lset >>= \(ReferenceSet _ refB2) -> Just (ReferenceSet refI (ReferenceBool nB valB : refB2))
      )
      where
        (ReferenceBool nB valB) = head refB
        xs = tail refB
astToRef (Define name (Ref s)) (ReferenceSet refI refB) lset =
  case valOfRefI s refI of
    Just x -> astToRef (Define name (Val x)) (ReferenceSet refI refB) lset
    Nothing -> case valOfRefB s refB of
      Nothing -> Just (ReferenceSet refI refB)
      Just x -> astToRef (Define name (Boolean x)) (ReferenceSet refI refB) lset
astToRef (Define name (Call x xs)) refs lset =
  case evalAST (Call x xs) lset of
    Just (Val y) -> astToRef (Define name (Val y)) refs lset
    Just (Boolean y) -> astToRef (Define name (Boolean y)) refs lset
    _ -> Nothing
astToRef _ _ _ = Nothing

argsFromRefs :: [Ast] -> ReferenceSet -> Maybe [Ast]
argsFromRefs [] _ = Just []
argsFromRefs ((Ref s) : xs) (ReferenceSet refI refB)
  | isInListI s refI =
      valOfRefI s refI >>= \valI ->
        argsFromRefs xs (ReferenceSet refI refB) >>= \resI ->
          Just (Val valI : resI)
  | isInListB s refB =
      valOfRefB s refB >>= \valB ->
        argsFromRefs xs (ReferenceSet refI refB) >>= \resB ->
          Just (Boolean valB : resB)
  | otherwise = Nothing
argsFromRefs (x : xs) ref = argsFromRefs xs ref >>= \res -> Just (x : res)

lambdaExist :: String -> [Ast] -> Bool
lambdaExist _ [] = False
lambdaExist name ((Lambda (Ref lname) _ _) : xs)
  | lname == name = True
  | otherwise = lambdaExist name xs
lambdaExist _ _ = False

astToLambdas :: Ast -> LambdaSet -> Maybe LambdaSet
astToLambdas (Lambda (Ref name) args procedure) (LambdaSet []) = Just (LambdaSet [Lambda (Ref name) args procedure])
astToLambdas (Lambda (Ref name) args procedure) (LambdaSet ((Lambda (Ref fname) params proc) : xs)) =
  if lambdaExist name (Lambda (Ref fname) params proc : xs)
    then
      ( if fname == name
          then Just (LambdaSet (Lambda (Ref name) args procedure : xs))
          else astToLambdas (Lambda (Ref name) args procedure) (LambdaSet xs)
      )
    else Just (LambdaSet (Lambda (Ref name) args procedure : Lambda (Ref fname) params proc : xs))
astToLambdas _ _ = Nothing

callArgsFromRefs :: Ast -> ReferenceSet -> Maybe Ast
callArgsFromRefs (Call cname cargs) ref =
  argsFromRefs cargs ref >>= \newArgs -> Just (Call cname newArgs)
callArgsFromRefs ast _ = Just ast

-- Run an AST data
runAST :: Ast -> ReferenceSet -> LambdaSet -> IO ()
-- Print a single value
runAST (Val x) _ _ = print x
-- Print a single boolean
runAST (Boolean x) _ _ = if x then putStrLn "#t" else putStrLn "#f"
-- Print the value or boolean of reference 's'
runAST (Ref s) (ReferenceSet refI refB) lset =
  case valOfRefI s refI of
    Nothing -> case valOfRefB s refB of
      Nothing -> putStrLn "*** ERROR : An unreferenced variable was called."
      Just x -> runAST (Boolean x) (ReferenceSet refI refB) lset
    Just x -> runAST (Val x) (ReferenceSet refI refB) lset
-- Call a defined function named 'name'
runAST (Call name args) ref lset = case argsFromRefs args ref of
  Nothing -> putStrLn "*** ERROR : one of the variables is not bound."
  Just args2 -> case evalAST (Call name args2) lset of
    Nothing -> putStrLn "*** ERROR : a Call error has occured."
    Just x -> runAST x ref lset
-- Call a condition
runAST (Condition c t e) ref lset = case callArgsFromRefs c ref of
  Nothing -> putStrLn "*** ERROR : one of the variables is not bound."
  Just c' -> case callArgsFromRefs t ref of
    Nothing -> putStrLn "*** ERROR : one of the variables is not bound."
    Just t' -> case callArgsFromRefs e ref of
      Nothing -> putStrLn "*** ERROR : one of the variables is not bound."
      Just e' -> case evalAST (Condition c' t' e') lset of
        Nothing -> putStrLn "*** ERROR : a Condition error has occured."
        Just x -> runAST x ref lset
-- Launch a procedure with 0 arguments, and 1 or more parameters
runAST (Procedure (_ : _) _ Nothing) _ _ = putStrLn "#procedure"
runAST _ _ _ = putStrLn "*** ERROR : Out of pattern."

-- Interpreter loop
runPrompt :: ReferenceSet -> LambdaSet -> IO ()
runPrompt refs lambdas = do
  line <- getLine
  case inputToCpt line of
    Nothing -> putStrLn "failed first"
    Just (cpt, _) -> case cptToAST cpt of
      Nothing -> putStrLn "failed second"
      Just ast -> case astToRef ast refs lambdas of
          Just newSet -> runPrompt newSet lambdas
          Nothing -> case astToLambdas ast lambdas of
              Just newLambdas -> runPrompt refs newLambdas
              Nothing -> runAST ast refs lambdas >> runPrompt refs lambdas
