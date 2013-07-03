{-# LANGUAGE DoAndIfThenElse #-}
module ThetaReduction(thetaReduction) where

import Control.Applicative
import Data.List

import Term
import VarEnvironment

data MarkedTerm = MLam Int String MarkedTerm
                | MApp MarkedTerm  MarkedTerm
                | MVar String
                --deriving Show

-- Uses alpha equivalence
instance Eq MarkedTerm where
   (MVar x) == (MVar y) = x == y
   (MApp m n) == (MApp p q) = m == p && n == q
   (MLam _ x m) == (MLam _ y n)
    | x == y = m == n
    | otherwise = m == (substitute (y `AssignTo` (MVar x)) n)
   _ == _ = False

-- TODO: Fix the repeated code for freeVars, substitute
instance TermClass MarkedTerm where
   freeVars (MLam _ x t) = filter (/= x) $ freeVars t
   freeVars (MApp x y) = nub $ freeVars x ++ freeVars y
   freeVars (MVar x) = [x]

   substitute (x `AssignTo` t) (MVar y)
    | x == y = t
    | otherwise = MVar y
   substitute (x `AssignTo` t) (MApp m n) = MApp (substitute (x `AssignTo` t) m) (substitute (x `AssignTo` t) n)
   substitute (x `AssignTo` t) (MLam i y m)
    | x == y = MLam i y m
    | otherwise = MLam i y (substitute (x `AssignTo` t) m)

------ 1. Computing term M1 ------

label :: Term -> [String] -> Int -> MarkedTerm
label (Var x) _ _ = MVar x
label (Lambda x m) acts i
 | x `elem` acts = MLam i x $ label m acts i
 | otherwise = MLam 1 x $ label m acts i
label (App m n) acts i = (label m acts i) `MApp` (label n (act n) 3)

markTerm :: Term -> MarkedTerm
markTerm t = label t (act t) 2

unmarkTerm :: MarkedTerm -> Term
unmarkTerm (MLam _ x m) = Lambda x (unmarkTerm m)
unmarkTerm (MApp m n) = App (unmarkTerm m) (unmarkTerm n)
unmarkTerm (MVar x) = Var x

------ 2. Computing term M2 ------

-- The order of thetas is because of theta2 being monadic
theta :: MarkedTerm -> Environment MarkedTerm
theta = theta2 . theta4 . theta3 . theta1
 where theta1 (((MLam 1 x n) `MApp` p) `MApp` q) =
          (MLam 1 x (n `MApp` q)) `MApp` p
       theta1 t = t
       theta2 (MLam 3 x ((MLam 1 y n) `MApp` p)) = do
          v <- newVar "var"
          w <- newVar "var"
          let n' = substitute (y `AssignTo` ((MVar v) `MApp` (MVar x))) n
              p' = substitute (x `AssignTo` (MVar w)) p
          return $ (MLam 1 v (MLam 3 x n')) `MApp` (MLam 3 w p')
       theta2 t = return t
       theta3 (n `MApp` ((MLam 1 x p) `MApp` q)) =
          (MLam 1 x (n `MApp` p)) `MApp` q
       theta3 t = t
       theta4 ((MLam 1 x (MLam 2 y n)) `MApp` p) =
          MLam 2 y $ (MLam 1 x n) `MApp` p
       theta4 t = t

thetaReduction' :: MarkedTerm -> Environment MarkedTerm
thetaReduction' t = do
   t' <- (thetaRec =<< theta t)
   if t /= t' then thetaReduction' t' else return t
 where thetaRec (MVar x) = return $ MVar x
       thetaRec (MApp m n) = MApp <$> (theta m >>= thetaRec) <*> (theta n >>= thetaRec)
       thetaRec (MLam i x m) = MLam i x <$> (theta m >>= thetaRec)

-- Point-free version of it is not nice.
thetaReduction :: Term -> Term
thetaReduction t = evalInEnvironment [] $ do
   t' <- markTerm <$> makeUniqueVars t
   unmarkTerm <$> thetaReduction' t'
