-----------------------------------------------------------------------------
--
-- Module      :  collect all files on disk
-- and compute for each the MD5
-- produces error: openFile: resource exhausted (Too many open files)
-- needs pipe for stream processing


-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , FlexibleContexts
    , TypeSynonymInstances
    , FlexibleInstances
    , ScopedTypeVariables
    , TypeFamilies
    #-}


module Lib.NoPipe where

import           Test.Framework

import qualified System.Directory as D
import qualified System.Posix as P
import   System.FilePath.Posix  ((</>))
import           Data.Digest.Pure.MD5  (md5)
import Safe
import qualified Data.ByteString.Lazy  as L
import Control.Exception (catch, SomeException)

recurseDir :: FilePath -> IO [String]
-- main entry point
recurseDir fp = do
    stat <- P.getFileStatus fp
    res1 <- if P.isRegularFile stat
        then do
            r <- processOneFile fp
            return [r]
        else do
            processDir fp
    return res1

processDir :: FilePath -> IO [String]
-- ^ process one directory
processDir dir = do
    -- process the dir as an entry
    res1 <- processOneDirEntry dir
    content3 <- directoryContent dir
    res3 <- mapM recurseDir content3
    return (res1 : concat res3)

processOneDirEntry :: FilePath -> IO String
processOneDirEntry dir = do
    let res1 = unwords ["\nD: ", dir]
    return res1

directoryContent  :: FilePath -> IO [String]
-- ^ find the entries in a directory - no recursion yet
directoryContent dir = do
--    putStrLn . unwords $ ["directoryContent - ", dir]
        stat <-   P.getSymbolicLinkStatus  (dir :: FilePath)
        isReadExecutable <-  P.fileAccess dir  True False True
        if isReadExecutable
            then do
                content <- D.getDirectoryContents dir
                -- exclude special files:
                let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
                -- complete the names to absolute filepath
                let content3 = map (dir </>) content2
                return content3
            else return []
    `catch` \(e::SomeException) -> do
                    putStrLn . unwords $ ["directoryContent exception for ", show e]
                    return []

processOneFile :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processOneFile fn = do
--    putStrLn . unwords $ ["processOneFile - ", fn]
    md <- getMD5 fn
    let res = unwords ["\nF:", fn, show md]
    return res

getMD5 :: FilePath -> IO (Maybe String)
-- ^ compute the MD5 value and return digest
getMD5 fn = do
        status <- P.getSymbolicLinkStatus fn
        let regular = P.isRegularFile status
        readable <- P.fileAccess fn True False False
        if regular && readable then do
                filedata <- L.readFile fn
                let res = show . md5 $ filedata
                return (Just res)
            else return Nothing
    `catch` \(e::SomeException) -> do
                    putStrLn . unwords $ ["getMD5 exception for ", show e]
                    return (Nothing)

testDir = "testDirFileIO"  -- relative path for test, gives relative path in output

test_1 = do
    res <- recurseDir testDir
    assertEqual resTestDir6 res

resTestDir6 =
    ["\nD:  testDirFileIO",
     "\nF: testDirFileIO/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
     "\nD:  testDirFileIO/subnew",
     "\nF: testDirFileIO/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
     "\nF: testDirFileIO/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
     "\nD:  testDirFileIO/sub.d",
     "\nF: testDirFileIO/sub.d/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
     "\nF: testDirFileIO/sub.d/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
     "\nF: testDirFileIO/sub.d/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
     "\nF: testDirFileIO/sub.d/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\"",
     "\nD:  testDirFileIO/sub.d/.hiddensub.d",
     "\nF: testDirFileIO/sub.d/.hiddensub.d/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
     "\nF: testDirFileIO/sub.d/.hiddensub.d/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
     "\nF: testDirFileIO/sub.d/.hiddensub.d/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
     "\nF: testDirFileIO/sub.d/.hiddensub.d/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\"",
     "\nF: testDirFileIO/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\""]

