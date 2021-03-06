{-# LANGUAGE DoAndIfThenElse #-}
module Type(Type(..), makeUniqueTVars, parse) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec as Parsec
import Control.Applicative hiding ((<|>))
import Control.Monad
import Data.List

import Term(TermClass(..), Substitution(..))
import VarEnvironment

data Type = TVar String
          | TArr Type Type
          | TForall String Type

instance Eq Type where
   (TVar x) == (TVar y) = x == y
   (TArr m n) == (TArr p q) = m == p && n == q
   (TForall x m) == (TForall y n)
    | x == y = m == n
    | otherwise = m == substitute (y `AssignTo` TVar x) n
   _ == _ = False

instance TermClass Type where
   freeVars (TForall x t) = filter (/= x) $ freeVars t
   freeVars (TArr x y) = nub $ freeVars x ++ freeVars y
   freeVars (TVar x) = [x]

   substitute (v `AssignTo` t) (TVar x)
    | v == x = t
    | otherwise = TVar x
   substitute (v `AssignTo` t) (TArr x y) = TArr (substitute (v `AssignTo` t) x) (substitute (v `AssignTo` t) y)
   substitute (v `AssignTo` t) (TForall x y)
    | v == x = TForall x y
    | otherwise = TForall x (substitute (v `AssignTo` t) y)

-- TODO: horrible repetition of makeUniqueVars
makeUniqueTVars :: (MonadEnvironment m) => Type -> m Type
makeUniqueTVars t = do
   mapM_ addToEnvironment $ freeVars t
   go t
 where go (TVar x) = return $ TVar x
       go (TArr m n) = TArr <$> go m <*> go n
       go (TForall x m) = do
          (x', m') <- renameVariable x m
          addToEnvironment x'
          TForall x' <$> go m'
       renameVariable x m = do
          inEnv <- inEnvironment x
          if inEnv then do
             y <- newVar "type"
             return (y, substitute (x `AssignTo` TVar y) m)
          else return (x, m)

------ Printing ------

brackets :: Int -> String -> String
brackets p str = if p > 0 then "(" ++ str ++ ")" else str

arrSymb :: String
arrSymb = "→"
--arrSymb = "->"
forallSymb :: String
forallSymb = "∀"
--forallSymb = "\\-/"

showType :: Int -> Type -> String
showType _ (TVar x) = x
showType prec (TArr t1 t2) = brackets (prec - 1) $
   showType 2 t1 ++ " " ++ arrSymb ++ " " ++ showType 1 t2
showType prec t@(TForall _ _) = brackets prec $ forallSymb ++ showForall t
 where showForall (TForall x n@(TForall _ _)) = x ++ " " ++ showForall n
       showForall (TForall x n) = x ++ ". " ++ showType 0 n
       showForall _ = error "showForall: Argument is not TForall. Couldn't happen."

instance Show Type where
   show = showType 0

------ Parsing ------
type Parser = Parsec String ()

varname :: Parser String
varname = many1 (alphaNum <|> oneOf "_")

bracketExpr :: Parser Type -> Parser Type
bracketExpr = between (char '(' *> spaces) (spaces *> char ')')

forallExpr :: Parser Type
forallExpr = do
   void $ try (string forallSymb) <|> string "\\-/"
   spaces
   vs <- many1 (varname <* spaces)
   void $ char '.'
   spaces
   t <- typeExpr
   return $ foldr TForall t vs

typeExpr :: Parser Type
typeExpr = try arrExpr <|> noArrExpr

noArrExpr :: Parser Type
noArrExpr = choice
   [ TVar <$> varname
   , forallExpr
   , bracketExpr typeExpr
   ]

arrExpr :: Parser Type
arrExpr = do
   t1 <- noArrExpr
   spaces
   void $ try (string arrSymb) <|> string "->"
   spaces
   t2 <- typeExpr
   return $ t1 `TArr` t2

fullExpr :: Parser Type
fullExpr = spaces *> typeExpr <* spaces <* eof

parse :: String -> Either ParseError Type
parse = Parsec.parse fullExpr ""
