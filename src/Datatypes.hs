{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Datatypes where

data Cpt
  = Nbr Int
  | Symbol String
  | List [Cpt]
  deriving (Show)

data Ast
  = Define String Ast
  | Call Ast [Ast]
  | Ref String
  | Val Int
  | Boolean Bool
  | -- params - call - maybe args
    Procedure [Ast] Ast (Maybe [Ast])
  | -- nom - params - procedure
    Lambda Ast [Ast] Ast
  | Condition Ast Ast Ast
  deriving (Show)

data ReferenceInt = ReferenceInt
  { nameI :: String,
    val :: Int
  }
  deriving (Show)

data ReferenceBool = ReferenceBool
  { nameB :: String,
    bool :: Bool
  }
  deriving (Show)

data ReferenceSet = ReferenceSet
  { setI :: [ReferenceInt],
    setB :: [ReferenceBool]
  }
  deriving (Show)

data LambdaSet = LambdaSet
  { setL :: [Ast]
  }
  deriving (Show)

createReferenceSet :: ReferenceSet
createReferenceSet = (ReferenceSet [] [])

createLambdaSet :: LambdaSet
createLambdaSet = (LambdaSet [])
