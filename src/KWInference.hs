{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module KWInference(TypeError(..), inferType) where
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Control.Applicative
import Control.Arrow

import Term
import Type
import ThetaReduction
import ASUP
import TypeError

------ Algorithm from Kfoury, Wells, "A Direct Algorithm for Type Inference in the ------
------ Rank-2 Fragment of the Second Order Lambda-Calculus", 1993.                 ------

------ The interface ------

checkUserTypes :: [(String ,Type)] -> Term -> TypeErrorMonad ()
-- TODO: should check that supplied types are for free or lambda-1 vars
-- TODO: should check that supplied types are rank-1
checkUserTypes userTypes term = return ()

inferType :: [(String, Type)] -> Term -> TypeErrorMonad Type
inferType userTypes term = do
   checkUserTypes userTypes term
   typeInASUP userTypes $ thetaReduction term
   {-let fvars = freeVars t-}
       {-avars = act t-}
       {-untypedFreeVars = fvars \\ map fst env-}
       {-typedOtherVars = map fst env \\ (fvars ++ avars)-}
       {-asup = computeASUPInstance env $ evalInEnvironment [] $ thetaReduction $ markTerm t-}
       {-(x, ineqs) = evalInEnvironment [] $ do-}
          {-(x, ineqs) <- asup-}
          {-ineqs' <- solveASUPInstance ineqs-}
          {-return (x, ineqs')-}
   {-in if not $ null untypedFreeVars then Left $ NotEnoughTypes untypedFreeVars-}
      {-else if not $ null typedOtherVars then Left $ TooMuchTypes typedOtherVars-}
      {-else maybe (Left Untypable) (Right . getOutputType (map (++ "_type0") avars) x) ineqs-}
