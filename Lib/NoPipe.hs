-----------------------------------------------------------------------------
--
-- Module      :  collect all files on disk
-- and compute for each the MD5

-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE
    MultiParamTypeClasses
    , FlexibleContexts
    , TypeSynonymInstances
    , FlexibleInstances
    , TypeFamilies
    #-}


module Lib.NoPipe where

import           Test.Framework


processFile :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processFile fn = do
    putStrLn . unwords $ ["processFile - ", fn]
    s <- readFile fn
    let md = 0
    let res = unwords ["F:", fn, show md]
    return res

processDir :: FilePath -> IO [String]
-- ^ process one directory
processDir dir = do
    putStrLn . unwords $ ["processDirectory - ", dir]

    let res1 = unwords ["D: ", dir]
    return [res1]



test_1 = do
    res <- processDir "testFileIO"
    assertEqual ["D:  testFileIO"] res




