module TestTheta(tests) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Term
import TestTerm()
import ThetaReduction

tests :: Test
tests = testGroup "Theta reduction tests"
   [ testProperty "theta form" prop_thetaForm
   , testProperty "theta beta equivalence" prop_thetaEquiv
   ]

-- This is dangerous since normal form might not exist.
-- TODO: Limit beta reduction to some large number of steps.
-- TODO: And when it exceeds that fail with exception(because there's no other way here)
prop_thetaEquiv :: Term -> Bool
prop_thetaEquiv t = (betaReduction $ thetaReduction t) == betaReduction t

prop_thetaForm :: Term -> Bool
prop_thetaForm = go 1 . thetaReduction
   where go :: Int -> Term -> Bool
         go 1 (Lambda _ m) = go 1 m
         go 1 m = go 2 m
         go 2 ((Lambda _ m) `App` n) = go 2 m && go 3 n
         go 2 m = go 4 m
         go 3 ((Lambda _ _) `App` _) = False
         go 3 (Var _) = True
         go 3 (Lambda _ n) = go 3 n
         go 3 (App n m) = go 3 n && go 3 m
         go 4 (Lambda _ _) = False
         go 4 n = go 3 n
         go _ _ = error "Impossible: only 1..5 states are used"
