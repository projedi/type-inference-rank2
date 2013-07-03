{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module TypeError(TypeError(..), TypeErrorMonad, untypable, notRank1Types, tooMuchTypes) where

import Control.Monad.Error

data TypeError = TooMuchTypes [String]
               | NotRank1Types [String]
               | Untypable
               | UnknownError String

instance Show TypeError where
   show (TooMuchTypes lst) = show lst ++ " are not free nor act and " ++ 
      "not allowed to have user types"
   show Untypable = "Term is untypable"
   show (NotRank1Types x) = "Variables " ++ show x ++ " is given not rank-1 type"
   show (UnknownError str) = "Unknown error: " ++ str

instance Error TypeError where
   strMsg = UnknownError

type TypeErrorMonad = Either TypeError

untypable :: (MonadError TypeError m, Monad m) => m a
untypable = throwError Untypable

notRank1Types :: (MonadError TypeError m, Monad m) => [String] -> m a
notRank1Types = throwError . NotRank1Types

tooMuchTypes :: (MonadError TypeError m) => [String] -> m a
tooMuchTypes = throwError . TooMuchTypes
