{-# LANGUAGE LambdaCase, DoAndIfThenElse, TupleSections, FlexibleContexts #-}
module ASUP(ASUP, ASUPRelation(..), TypedTerm(..), typeTerm, typeInASUP, computeASUP, solveASUP, solveASUPInEnvironment) where

import Control.Applicative
import Control.Monad.Error
import Data.List
import qualified Data.Map as Map
import Data.Array(Array, (!), (//))
import qualified Data.Array as Array
import Data.Either (partitionEithers)
import Control.Arrow(first)

import Term
import Type
import TypeError
import VarEnvironment

data TypedTerm = TypedVar Type String
               | TypedApp Type TypedTerm TypedTerm
               | TypedLam Type (String, Type) TypedTerm
               deriving Show

instance TermClass TypedTerm where
   freeVars (TypedVar _ x) = [x]
   freeVars (TypedApp _ n m) = freeVars n `union` freeVars m
   freeVars (TypedLam _ (x,_) n) = freeVars n \\ [x]

   substitute = undefined

allTypeVars :: TypedTerm -> [String]
allTypeVars (TypedVar t _) = freeVars t
allTypeVars (TypedApp t n m) = freeVars t `union` allTypeVars n `union` allTypeVars m
allTypeVars (TypedLam t (_,xt) n) = freeVars t `union` freeVars xt `union` allTypeVars n

typeOf :: TypedTerm -> Type
typeOf (TypedVar t _) = t
typeOf (TypedApp t _ _) = t
typeOf (TypedLam t _ _) = t

newTVarName :: MonadEnvironment m => m String
newTVarName = newVar "type"

newTVar :: MonadEnvironment m => m Type
newTVar = TVar <$> newTVarName

typeTerm :: MonadEnvironment m => Term -> m TypedTerm
typeTerm (Var x) = flip TypedVar x <$> newTVar
typeTerm (Lambda x n) = do
   xType <- newTVar
   flip TypedLam (x, xType) <$> newTVar <*> typeTerm n
typeTerm (App n m) = TypedApp <$> newTVar <*> typeTerm n <*> typeTerm m

type ASUP = Array Int [ASUPRelation]

data ASUPRelation = EqualTo Type Type
                  | LessThan Type Type
                  deriving (Eq, Show)

polyVarName :: String -> Int -> String
polyVarName x i = x ++ "_type_" ++ show i

polyVarType :: String -> Int -> Type
polyVarType = (TVar .) . polyVarName

-- Assumes userTypes is good
computeASUP :: [(String, Type)] -> TypedTerm -> ASUP
computeASUP userTypes term = Array.array (0,length columns - 1) columns
 where zeroColumn = (0,
          map (\(x,t) -> polyVarType x 0 `EqualTo` t) userTypes
          ++ lam2Types term)
       lam2Types (TypedLam _ (x,tx) n) = polyVarType x 0 `EqualTo` tx : lam2Types n
       lam2Types _ = []
       nColumn polyVars yn ynt tn n = (n, 
          map (\x -> polyVarType x (n - 1) `LessThan` polyVarType x n) polyVars
          ++ [polyVarType yn n `EqualTo` ynt]
          ++ [polyVarType yn n `EqualTo` typeOf tn] ++ goSubTerm [] n tn)
       lastColumn polyVars tn n = (n,
          map (\x -> polyVarType x (n - 1) `LessThan` polyVarType x n) polyVars
          ++ goSubTerm [] n tn)
       goSubTerm lamVars i (TypedVar t x) =
          case lookup x lamVars of
           Nothing -> [polyVarType x (i - 1) `LessThan` t]
           Just tx -> [tx `EqualTo` t]
       goSubTerm lamVars i (TypedApp t n m) =
          [typeOf n `EqualTo` (typeOf m `TArr` t)]
          ++ goSubTerm lamVars i n ++ goSubTerm lamVars i m
       goSubTerm lamVars i (TypedLam t (x,tx) n) =
          [t `EqualTo` (tx `TArr` typeOf n)]
          ++ goSubTerm ((x,tx):lamVars) i n
       go polyVars i (TypedLam _ (x,_) n) = go (x:polyVars) i n
       go polyVars i (TypedApp _ (TypedLam _ (y,yt) n) m) = 
          nColumn polyVars y yt m i : go (y:polyVars) (i + 1) n
       go polyVars i t = [lastColumn polyVars t i]
       columns = zeroColumn : go (freeVars term) 1 term

redex1 :: (MonadEnvironment m) => ASUPRelation -> m (Maybe (Substitution Type))
redex1 (_ `EqualTo` _) = return Nothing
redex1 (TVar _ `LessThan` _) = return Nothing
redex1 (t `LessThan` TVar x) = (Just . (x `AssignTo`)) <$> renameVariables t
 where renameVariables (TVar _) = newTVar
       renameVariables (t1 `TArr` t2) =
          TArr <$> renameVariables t1 <*> renameVariables t2
       renameVariables (TForall _ _) = error "redex1: relation with not rank-0 type"
redex1 ((t1 `TArr` t2) `LessThan` (t3 `TArr` t4)) =
   maybe (redex1 (t2 `LessThan` t4)) (return . Just) =<< redex1 (t1 `LessThan` t3)
redex1 ((TForall _ _) `LessThan` _) = error "redex1: relation with not rank-0 type"
redex1 (_ `LessThan` (TForall _ _)) = error "redex1: relation with not rank-0 type"

redex2 :: MonadError TypeError m => ASUPRelation -> m (Maybe (Substitution Type))
redex2 (TVar x `EqualTo` t)
 | TVar x == t = return Nothing
 | x `elem` freeVars t = untypable
 | otherwise = return $ Just $ x `AssignTo` t
redex2 (t `EqualTo` TVar x) = redex2 (TVar x `EqualTo` t)
redex2 ((TArr t1 t2) `EqualTo` (TArr t3 t4)) =
   maybe (redex2 (t2 `EqualTo` t4)) (return . Just) =<< redex2 (t1 `EqualTo` t3)
redex2 (a `LessThan` b) =
   case Map.elems $ Map.filter ((>1) . length) $ findPaths a b of
    [] -> return Nothing
    (m:n:_):_ -> redex2 (m `EqualTo` n)
    _:_ -> error "Impossible: the first element must have at least two elements because of filter"
 where findPaths (TVar x) t = Map.singleton x [t]
       findPaths _ (TVar _) = Map.empty
       findPaths (t1 `TArr` t2) (t3 `TArr` t4) =
          Map.unionWith union (findPaths t1 t3) (findPaths t2 t4)
       findPaths (TForall _ _) _ = error "redex2: relation with not rank-0 type"
       findPaths _ (TForall _ _) = error "redex2: relation with not rank-0 type"
redex2 ((TForall _ _) `EqualTo` _) = error "redex2: relation with not rank-0 type"
redex2 (_ `EqualTo` (TForall _ _)) = error "redex2: relation with not rank-0 type"

redexColumn :: (MonadEnvironment m, MonadError TypeError m) => [ASUPRelation] -> m (Maybe (Substitution Type))
redexColumn [] = return Nothing
redexColumn (rel:rels) = do
   r1 <- redex1 rel
   r2 <- redex2 rel
   case (r1,r2) of
    (Just _,_) -> return r1
    (_, Just _) -> return r2
    _ -> redexColumn rels

redexASUP :: (MonadEnvironment m, MonadError TypeError m) => ASUP -> m (Maybe (Int, Substitution Type))
redexASUP = go . Array.assocs
 where go [] = return Nothing
       go ((n,rels):cols) =
          maybe (go cols) (return . Just . (n,)) =<< (redexColumn rels)

solveASUP :: (MonadEnvironment m, MonadError TypeError m) => ASUP -> m [Substitution Type]
solveASUP = go []
 where go subs asup = do
          res <- redexASUP asup
          case res of
           Nothing -> return subs
           Just (n,sub) -> go (sub:subs) $ substituteInASUP n sub asup

substituteInASUP :: Int -> Substitution Type -> ASUP -> ASUP
substituteInASUP n sub asup
 | n == snd (Array.bounds asup) =
      let newRight = substituteRight (asup ! n)
      in asup // [(n, newRight)]
 | otherwise =
      let newRight = substituteRight (asup ! n)
          newLeft = substituteLeft (asup ! (n + 1))
      in asup // [(n, newRight), (n+1, newLeft)]
 where substituteRight [] = []
       substituteRight ((t1 `EqualTo` t2):rels) =
          (substitute sub t1 `EqualTo` substitute sub t2) : substituteRight rels
       substituteRight ((t1 `LessThan` t2):rels) =
          (t1 `LessThan` substitute sub t2) : substituteRight rels
       substituteLeft [] = []
       substituteLeft ((t1 `EqualTo` t2):rels) = (t1 `EqualTo` t2) : substituteLeft rels
       substituteLeft ((t1 `LessThan` t2):rels) = (substitute sub t1 `LessThan` t2) : substituteLeft rels

substituteInTerm :: TypedTerm -> [Substitution Type] -> TypedTerm
substituteInTerm (TypedVar t x) subs = TypedVar (substituteAll subs t) x
substituteInTerm (TypedApp t n m) subs =
   TypedApp (substituteAll subs t) (substituteInTerm n subs) (substituteInTerm m subs)
substituteInTerm (TypedLam t (x,tx) n) subs =
   TypedLam (substituteAll subs t) (x,substituteAll subs tx) (substituteInTerm n subs)

makeRank0Types :: (MonadError TypeError m, MonadEnvironment m) => [(String, Type)] -> m [(String, Type)]
makeRank0Types userTypes = do
   userTypes' <- forM userTypes $ \(x,t) ->
      (x,) <$> makeUniqueTVars t
   case partitionEithers $ map (uncurry stripForall) userTypes' of
    ([], userTypes'') -> return userTypes''
    (lst,_) -> notRank1Types lst
 where stripForall y (TForall _ t) = stripForall y t
       stripForall y t = if checkRank0 t then Right (y,t) else Left y
       checkRank0 (TVar _) = True
       checkRank0 (n `TArr` m) = checkRank0 n && checkRank0 m
       checkRank0 (TForall _ _) = False

solveASUPInEnvironment :: [(String, Type)] -> TypedTerm -> ASUP -> TypeErrorMonad [Substitution Type]
solveASUPInEnvironment userTypes t asup = 
   let initEnvironment = allTypeVars t ++ concatMap (freeVars . snd) userTypes
   in evalInEnvironmentT initEnvironment $ solveASUP asup

extractType :: [(String, Type)] -> TypedTerm -> Type
extractType userTypes term = foldr TArr mainType lam2Types
 where (lam2, mainType) = go term
       lam2Types = map (\x -> maybe (TForall "a" $ TVar "a") id $ lookup x userTypes) lam2
       go (TypedLam _ (x,_) n) = first (x:) $ go n
       go t = ([],typeOf t)

typeInASUP :: [(String, Type)] -> Term -> TypeErrorMonad Type
typeInASUP userTypes t = do
   let excessiveTypes = map fst userTypes \\ (act t ++ freeVars t)
       initEnvironment = concatMap (freeVars . snd) userTypes
   unless (null excessiveTypes) $
      tooMuchTypes excessiveTypes
   evalInEnvironmentT initEnvironment $ do
      t' <- typeTerm t
      userTypes' <- makeRank0Types userTypes
      let asup = computeASUP userTypes' t'
      subs <- solveASUP asup
      let t'' = substituteInTerm t' subs
      return $ extractType userTypes t'' -- userTypes because we need those forall qualifiers
