{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module VarEnvironment( Environment
                     , EnvironmentT
                     , MonadEnvironment(..)
                     , evalInEnvironment
                     , evalInEnvironmentT
                     ) where

import Control.Monad.State (StateT, evalStateT, MonadTrans)
import qualified Control.Monad.State as State
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans.State (liftCatch)
import Control.Monad.Error(MonadError(..))
import Control.Monad.Trans(MonadTrans(..))
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow(first)

newtype EnvironmentT m a = EnvironmentT {runEnvironment :: StateT (Set String, Integer) m a}
                         deriving (Monad, Functor, Applicative, MonadTrans)

type Environment = EnvironmentT Identity

evalInEnvironment :: [String] -> Environment a -> a
evalInEnvironment env m = runIdentity $ evalInEnvironmentT env m

evalInEnvironmentT :: (Monad m) => [String] -> EnvironmentT m a -> m a
evalInEnvironmentT env (EnvironmentT m) = evalStateT m (Set.fromList env, 0)

class (Monad m, Functor m, Applicative m) => MonadEnvironment m where
   addToEnvironment :: String -> m ()
   inEnvironment :: String -> m Bool
   newVar :: String -> m String

instance (Monad m, Functor m) => MonadEnvironment (EnvironmentT m) where
   addToEnvironment = EnvironmentT . State.modify . first . Set.insert
   inEnvironment x = EnvironmentT $ (Set.member x . fst) <$> State.get
   newVar prefix = EnvironmentT $ do
      (env, counter) <- State.get
      let (i, v) = head $ filter (not . flip Set.member env . snd)
                        $ map (\x -> (x, prefix ++ "_" ++ show x)) [counter..]
      State.put (env, i + 1)
      return v

instance (MonadError e m) => MonadError e (EnvironmentT m) where
   throwError = lift . throwError
   catchError (EnvironmentT m) handler =
      EnvironmentT $ liftCatch catchError m (runEnvironment . handler)
