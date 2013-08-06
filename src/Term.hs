{-# LANGUAGE DoAndIfThenElse #-}
module Term(Term(..), TermClass(..), Substitution(..), parse, act, makeUniqueVars, betaReduction) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List

import VarEnvironment

data Substitution a = AssignTo String a
                    deriving Show

instance Functor Substitution where
   fmap f (x `AssignTo` t) = x `AssignTo` f t

class TermClass t where
   freeVars :: t -> [String]
   substitute :: Substitution t -> t -> t
   substituteAll :: (Eq t) => [Substitution t] -> t -> t
   substituteAll subs t =
    if t == t' then t else substituteAll subs t'
    where t' = foldr substitute t subs

data Term = Lambda String Term
          | App Term Term
          | Var String

instance Eq Term where
   (Var x) == (Var y) = x == y
   (App n1 m1) == (App n2 m2) = n1 == n2 && m1 == m2
   (Lambda x n) == (Lambda y m)
    | x == y = n == m
    | otherwise = n == substitute (y `AssignTo` Var x) m
   _ == _ = False

instance TermClass Term where
   freeVars (Lambda x t) = filter (/= x) $ freeVars t
   freeVars (App x y) = nub $ freeVars x ++ freeVars y
   freeVars (Var x) = [x]

   substitute (v `AssignTo` t) (Var x)
    | v == x = t
    | otherwise = Var x
   substitute (v `AssignTo` t) (App x y) = App (substitute (v `AssignTo` t) x) (substitute (v `AssignTo` t) y)
   substitute (v `AssignTo` t) (Lambda x y)
    | v == x = Lambda x y
    | otherwise = Lambda x (substitute (v `AssignTo` t) y)

act :: Term -> [String]
act (Var _) = []
act (Lambda x m) = x : act m
act (App m _)
 | null (act m) = []
 | otherwise = tail (act m)

betaReduction :: Term -> Term
betaReduction t = evalInEnvironment (freeVars t) (betaReduction' <$> makeUniqueVars t)
 where betaReduction' (Lambda x n `App` m) = betaReduction' $ substitute (x `AssignTo` m) n
       betaReduction' (Var x) = Var x
       betaReduction' (Lambda x n) = Lambda x $ betaReduction' n
       betaReduction' (App n m) =
          let n' = betaReduction' n
          in if n == n' then App n $ betaReduction' m
             else betaReduction' (App n' m)

makeUniqueVars :: Term -> Environment Term
makeUniqueVars t = do
   mapM_ addToEnvironment $ freeVars t
   go t
 where go (Var x) = return $ Var x
       go (App m n) = App <$> go m <*> go n
       go (Lambda x m) = do
          (x', m') <- renameVariable x m
          addToEnvironment x'
          Lambda x' <$> go m'
       renameVariable x m = do
          inEnv <- inEnvironment x
          if inEnv then do
             y <- newVar "var"
             return (y, substitute (x `AssignTo` Var y) m)
          else return (x, m)

------ Printing ------
brackets :: Int -> String -> String
brackets p str = if p > 0 then "(" ++ str ++ ")" else str

lambdaSymb :: String
lambdaSymb = "λ"
--lambdaSymb = "\\"

showTerm :: Int -> Term -> String
showTerm _ (Var x) = x
showTerm prec (App t1 t2) = brackets (prec - 1) (showTerm 1 t1 ++ " " ++ showTerm 2 t2)
showTerm prec t@(Lambda _ _) =
   brackets prec $ lambdaSymb ++ showLambda t
 where showLambda (Lambda x n@(Lambda _ _)) = x ++ " " ++ showLambda n
       showLambda (Lambda x n) = x ++ ". " ++ showTerm 0 n
       showLambda _ = error "showLambda: Argument is not Lambda. Couldn't happen."

instance Show Term where
   show = showTerm 0

------ Parsing ------
type Parser = Parsec String ()

varname :: Parser String
varname = many1 (alphaNum <|> oneOf "_")

bracketExpr :: Parser Term -> Parser Term
bracketExpr = between (char '(' *> spaces) (spaces *> char ')')

lambdaExpr :: Parser Term
lambdaExpr = do
   void $ char '\\' <|> char 'λ'
   spaces
   vs <- many1 (varname <* spaces)
   void $ char '.'
   spaces
   t <- termExpr
   return $ foldr Lambda t vs

termExpr :: Parser Term
termExpr = try appExpr <|> noAppTermExpr

noAppTermExpr :: Parser Term
noAppTermExpr = choice
   [ lambdaExpr -- Precedes variable parser because λ is a proper variable name
   , Var <$> varname
   , bracketExpr termExpr
   ]

appExpr :: Parser Term
appExpr = do
   t1 <- noAppTermExpr
   ts <- many1 (spaces *> noAppTermExpr)
   return $ foldl App t1 ts

fullExpr :: Parser Term
fullExpr = spaces *> termExpr <* spaces <* eof

parse :: String -> Either ParseError Term
parse = Parsec.parse fullExpr ""
