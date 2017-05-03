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
    #-}


module Lib.WithPipe where

import           Test.Framework

import qualified System.Directory as D
import qualified System.Posix as P
import   System.FilePath.Posix  ((</>))
import           Data.Digest.Pure.MD5  (md5)
import Safe
import qualified Data.ByteString.Lazy  as L
import Lib.NoPipe (getMD5, res_6a)

processOneDirEntry :: FilePath -> IO [String]
processOneDirEntry fp = do
    stat <- P.getFileStatus fp
    res1 <- if P.isRegularFile stat
        then do
            processFile fp
        else do
            processDir fp
--    let res2 =   unwords ["\n?: ", fp]

    return res1


processFile :: FilePath -> IO [String]
-- ^ process one file - print filename as a stub
processFile fn = do
    putStrLn . unwords $ ["processFile - ", fn]
    s <- readFile fn
    md <- getMD5 fn
    let res = unwords ["\nF:", fn, show md]
    return [res]


processDir :: FilePath -> IO [String]
-- ^ process one directory
processDir dir = do
    putStrLn . unwords $ ["processDirectory - ", dir]
    let res1 = unwords ["\nD: ", dir]
    content <- D.getDirectoryContents dir
    -- exclude special files:
    let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
    -- complete the names to absolute filepath
    let content3 = map (dir </>) content2
    -- process each entry
    res3 <- mapM processOneDirEntry content3
    return (res1 : concat res3)



test_1 = do
    res <- processDir "/home/frank/Workspace8/testDirFileIO"
    assertEqual res_6a res



