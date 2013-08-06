module KWInference(TypeError(..), inferType) where

import Control.Applicative
import Data.Char
import Data.List

import Term
import Type
import ThetaReduction
import ASUP
import TypeError

generateSubs :: [String] -> [String] -> [Substitution Type]
generateSubs taken = go "a"
 where go _ [] = []
       go x (v:vs)
        | x `elem` taken = go (next x) (v:vs)
        | otherwise = (v `AssignTo` TVar x) : go (next x) vs
       next x
        | last x == 'z' = replicate (length x + 1) 'a'
        | otherwise = init x ++ [chr (ord (last x) + 1)]

prettifyType :: [(String, Type)] -> Type -> Type
prettifyType userTypes t = substituteAll subs t
 where subs = generateSubs ftvUser (freeVars t \\ ftvUser)
       ftvUser = foldr union [] $ map (freeVars . snd) userTypes

inferType :: [(String, Type)] -> Term -> TypeErrorMonad Type
inferType userTypes term = do
   prettifyType userTypes <$> typeInASUP userTypes (thetaReduction term)
