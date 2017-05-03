-----------------------------------------------------------------------------
--
-- Module      :  explanation

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , FlexibleContexts
    , TypeSynonymInstances
    , FlexibleInstances
    , TypeFamilies
    , ScopedTypeVariables
    #-}


module Lib.MonadTransformer where

import           Test.Framework
import qualified Data.Map  as Map
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity


type ErrOrVal = Either String

type ErrIO  = ErrorT String IO
-- an instance of Control.Monad.Error for ErrIO is automatic

-- | runErr to avoid the depreceated message for runErrorT, which is identical
runErr :: ErrIO a -> IO (ErrOrVal a)
runErr = runErrorT

callIO :: IO a -> ErrorT String IO a
callIO op = do
    liftIO op  `catchError` (\e -> throwError  e)


type DBstate  = StateT (Map.Map Int String) Identity

-- use stateMonad for the MPstore

class MPstoreTFstate db  where
    type KeyF db
    type ValueF db

    empty :: db  ()
    add :: KeyF db -> ValueF db ->  db ()
    find :: KeyF db -> db ( Maybe (ValueF db))

instance MPstoreTFstate (DBstate ) where
    type KeyF (DBstate ) = Int
    type ValueF (DBstate ) = String

    empty = do
                put Map.empty
                return ()
    add k v = do
                s <- get
                let s1 = Map.insert k v s
                put s1
                return ()
    find k = do
                s :: (Map.Map Int String) <- get
                let v :: Maybe String = Map.lookup k s
                return v

test_FP1g = assertEqual (Just "val 1") v
    where
        (v, s) = runState (db1 >> db2) (Map.empty)
        db2 = find 1 :: DBstate (Maybe String)
        db1 = add 1 "val 1" :: DBstate  ()
        db0 = empty :: DBstate ()
