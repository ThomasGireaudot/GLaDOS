module SharedFunc where

import Datatypes
import Running
import Datatypes
import Parsers

instance Eq ReferenceBool where
  (ReferenceBool nB valB) == (ReferenceBool nB' valB') = nB == nB' && valB == valB'

instance Eq ReferenceInt where
  (ReferenceInt nB valB) == (ReferenceInt nB' valB') = nB == nB' && valB == valB'

instance Eq ReferenceSet where
  (ReferenceSet nB valB) == (ReferenceSet nB' valB') = nB == nB' && valB == valB'

instance Eq Ast where
  (Define a b) == (Define c d) = a == c && b == d
  (Call a b) == (Call c d) = a == c && b == d
  (Ref a) == (Ref b) = a == b
  (Val a) == (Val b) = a == b
  (Boolean a) == (Boolean b) = a == b
  (Procedure a b c) == (Procedure d e f) = a == d && b == e && c == f
  (Lambda a b c) == (Lambda d e f) = a == d && b == e && c == f
  (Condition a b c) == (Condition d e f) = a == d && b == e && c == f
  _ == _ = False

instance Eq Cpt where
  (Nbr x) == (Nbr y) = x == y
  (Symbol x) == (Symbol y) = x == y
  (List xs) == (List ys) = xs == ys
  _ == _ = False



instance Eq LambdaSet where
  (LambdaSet l1) == (LambdaSet l2) = l1 == l2

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False