-----------------------------------------------------------------------------
--
-- Module      :  collect all files on disk
-- and compute for each the MD5
-- with pipe


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


module Lib.WithPipe where

import           Test.Framework

import qualified Pipes as Pipe
--import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))

import qualified System.Directory as D
import qualified System.Posix as Posix
import   System.FilePath.Posix  ((</>))
import Lib.NoPipe (getMD5)

startPipe :: FilePath ->  IO String
-- ^ collect the filenames and md5
startPipe fp = do
    res <- Pipe.runEffect $  effect9 fp
    return "test" -- res

effect9 :: FilePath -> Pipe.Effect IO ()  -- useful to fix types
effect9 fp =  Pipe.for (initialProducer fp) $ \dir ->
                processOneDirEntry dir
                >-> finalConsumer0

--initialProducer :: FilePath -> Pipe.Producer String IO ()
initialProducer fp = do
    Pipe.yield fp

--finalConsumer :: String -> Consumer String IO ()
finalConsumer0 = do
    st <- Pipe.await
    Pipe.lift $ putStrLn st
    finalConsumer0


--processOneDirEntry :: FilePath -> Pipe FilePath String IO ()
-- must not have a defined type
processOneDirEntry fp = do
    stat <- Pipe.lift $ Posix.getFileStatus fp
    let isRegular = Posix.isRegularFile stat
    if isRegular
        then do
            res <- Pipe.lift $ processFile1 fp
            Pipe.yield res
        else do
            let res1 = unwords ["\nD: ", fp]
            Pipe.yield res1
            content ::[FilePath]  <- Pipe.lift $ processDir1 fp
--            lift $ putStrLn . show $ content
            mapM_ processOneDirEntry content  --   recursion
    return ()

----------------------------------------------support code (not pipes)
processFile1 :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processFile1 fn = do
    md <-  getMD5 fn
    let res = unwords ["F:", fn, show md]
    return res

--processDir1 :: FilePath -> IO [FilePath ]
-- ^ process one directory
processDir1 dir = do
    let res1 = unwords ["D: ", dir]
    content <- D.getDirectoryContents dir
    -- exclude special files:
    let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
    -- complete the names to absolute filepath
    let content3 = map (dir </>) content2
    -- process each entry
    return content3


test_1 = do
    putStrLn "----------------------------"
    startPipe "/home/frank/Workspace8/testDirFileIO"
    putStrLn "============================"
    assertBool False  -- to force output


