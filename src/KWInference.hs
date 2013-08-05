module KWInference(TypeError(..), inferType) where

import Term
import Type
import ThetaReduction
import ASUP
import TypeError

inferType :: [(String, Type)] -> Term -> TypeErrorMonad Type
inferType userTypes term = do
   typeInASUP userTypes $ thetaReduction term
