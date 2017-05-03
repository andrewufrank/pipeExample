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
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>), Pipe (..))

import qualified System.Directory as D
import qualified System.Posix as P
import   System.FilePath.Posix  ((</>))
--import Safe
--import qualified Data.ByteString.Lazy  as L
import Lib.NoPipe (getMD5, res_6a)
import Data.Traversable

startPipe :: FilePath ->  IO String
-- ^ collect the filenames and md5
startPipe fp = do
    res <- Pipe.runEffect $
        initialProducer fp
        ~>  processDir
        ~> processOneDirEntry
        >-> Pipe.print
--        >-> toListM
    return "test" -- res

initialProducer :: FilePath -> Pipe.Producer String IO ()
initialProducer fp = do
    Pipe.yield fp
    return ()

--processOneDirEntry :: FilePath -> IO [String]
processOneDirEntry ::  Pipe FilePath String IO ()
processOneDirEntry  = do
    fp <- Pipe.await
    stat <- Pipe.lift $ P.getFileStatus fp
    let isRegular = P.isRegularFile stat
    if isRegular
        then do
            res <- Pipe.lift $ processFile fp
            Pipe.yield res
        else do
            (do
                Pipe.yield fp)
                processDir )
    return ()


processFile :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processFile fn = do
--    putStrLn . unwords $ ["processFile - ", fn]
--    s <- readFile fn
    md <-  getMD5 fn
    let res = unwords ["\nF:", fn, show md]
    return res


--processDir :: FilePath -> IO [String]
-- ^ process one directory
processDir ::     Pipe.Pipe String FilePath IO ()
processDir  = do
--    putStrLn . unwords $ ["processDirectory - ", dir]
    dir <- Pipe.await
    content ::[FilePath]  <- Pipe.lift $ processDir1 dir

    Pipe.each content -- processOneDirEntry

    return () -- yRes

--processDir1 :: FilePath -> IO [FilePath ]
-- ^ process one directory
processDir1 dir = do
    let res1 = unwords ["\nD: ", dir]
    content <- D.getDirectoryContents dir
    -- exclude special files:
    let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
    -- complete the names to absolute filepath
    let content3 = map (dir </>) content2
    -- process each entry
    return content3


--test_1 = do
--    res <- processDir "/home/frank/Workspace8/testDirFileIO"
--    assertEqual res_6a res



