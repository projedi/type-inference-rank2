module TestASUP(tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Data.Array
import Data.List

import ASUP
import Term
import TestTerm()
import ThetaReduction
import Type
import VarEnvironment

tests :: Test
tests = testGroup "ASUP tests"
   [ testProperty "Generated ASUP is indeed ASUP" prop_asupCorrectness
   , testProperty "ASUP solver solves ASUP(If reported untypable, returns True)" prop_asupSolver
   ]

prop_asupCorrectness :: Term -> Bool
prop_asupCorrectness t = not $ any (not . null) $ intersects allVars
 where t' = thetaReduction t
       t'' = evalInEnvironment [] $ typeTerm t'
       asup = computeASUP [] t''
       intersects [] = []
       intersects (x:xs) = intersections (x:xs) xs
       intersections [] _ = []
       intersections [_] _ = []
       intersections (_:x:xs) [] = intersections (x:xs) xs
       intersections (x:xs) (y:ys) = intersect x y : intersections (x:xs) ys
       getLeftVars ((_ `EqualTo` _):xs) = getLeftVars xs
       getLeftVars ((x `LessThan` _):xs) = freeVars x `union` getLeftVars xs
       getLeftVars [] = []
       getRightVars ((x1 `EqualTo` x2):xs) = freeVars x1 `union` freeVars x2 `union` getRightVars xs
       getRightVars ((_ `LessThan` x):xs) = freeVars x `union` getRightVars xs
       getRightVars [] = []
       varCount = 1 + snd (bounds asup)
       vars i
        | i == 0 = getLeftVars (asup ! i)
        | i == varCount = getRightVars (asup ! (i - 1))
        | otherwise = getRightVars (asup ! (i - 1)) `union` getLeftVars (asup ! i)
       allVars = [vars i | i <- [0..varCount]]

-- TODO: Generate random asup
prop_asupSolver :: Term -> Bool
prop_asupSolver term =
   case solveASUPInEnvironment [] t asup of
    Left _ -> True -- Cannot check if it is indeed untypable
    Right subs -> check subs $ concat $ elems asup
 where t = evalInEnvironment [] $ typeTerm $ thetaReduction term
       asup = computeASUP [] t
       check _ [] = True
       check subs (rel:rels) = checkRelation subs rel && check subs rels
       checkRelation subs (t1 `LessThan` t2) = checkLT (substituteAll subs t1) (substituteAll subs t2)
       checkRelation subs (t1 `EqualTo` t2) = checkET (substituteAll subs t1) (substituteAll subs t2)
       checkET (TVar x) (TVar y) = x == y
       checkET (TArr n m) (TArr p q) = checkET n p && checkET m q
       checkET _ _ = False
       checkLT (TVar _) (TVar _) = True
       checkLT (TVar _) _ = True
       checkLT _ (TVar _) = False
       checkLT (TArr m n) (TArr p q) = checkLT m p && checkLT n q
       checkLT _ _ = False
