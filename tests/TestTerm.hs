{-# OPTIONS_GHC -fno-warn-orphans #-}
module TestTerm(tests) where

import Control.Applicative
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Term
import VarEnvironment

tests :: Test
tests = testGroup "Term tests"
   [ testProperty "unique vars equivalence" prop_uniqueVarsEquiv
   ]

{- -- This instance takes a lot of time
instance Arbitrary Term where
   arbitrary = do
      n <- choose (0, 2) :: Gen Int
      case n of
       0 -> arbitraryLambda
       1 -> arbitraryApp
       2 -> arbitraryVar
    where arbitraryLambda = Lambda <$> elements vars <*> arbitrary
          arbitraryApp = App <$> arbitrary <*> arbitrary
          arbitraryVar = Var <$> elements vars
          vars = ["a", "b", "c", "d"]
-}

-- TODO: Generated more balanced(variable-wise) terms
instance Arbitrary Term where
   arbitrary = do
      depth <- choose (depthMin, depthMax) :: Gen Int
      varsCount <- choose (varsMin, varsMax) :: Gen Int
      go (generateVars varsCount) depth
    where depthMin = 3
          depthMax = 10
          varsMin = 4
          varsMax = 10
          generateVars n = map (("a_"++) . show) [1..n]
          go vars 0 = arbitraryVar vars
          go vars n = do
             k <- choose (0, 2) :: Gen Int
             case k of
              0 -> arbitraryLambda vars n
              1 -> arbitraryApp vars n
              2 -> arbitraryVar vars
              _ -> error "Impossible: not in (0,2) range"
          arbitraryLambda vars n = Lambda <$> elements vars <*> go vars (n - 1)
          arbitraryApp vars n = App <$> go vars (n - 1) <*> go vars (n - 1)
          arbitraryVar vars = Var <$> elements vars

prop_uniqueVarsEquiv :: Term -> Bool
prop_uniqueVarsEquiv t = evalInEnvironment (freeVars t) (makeUniqueVars t) == t
