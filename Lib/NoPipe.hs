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

import System.Directory

processFile :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processFile fn = do
    putStrLn . unwords $ ["processFile - ", fn]
    s <- readFile fn
    let md = 0
    let res = unwords ["\nF:", fn, show md]
    return res

processDir :: FilePath -> IO [String]
-- ^ process one directory
processDir dir = do
    putStrLn . unwords $ ["processDirectory - ", dir]
    let res1 = unwords ["\nD: ", dir]
    content <- getDirectoryContents dir
    let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
    let res2 = map (\fs -> unwords ["\n?: ", fs]) content2
    return (res1 : res2)



test_1 = do
    res <- processDir "/home/frank/testFileIO"
    assertEqual res_2a res

res_2a =
    ["\nD:  /home/frank/testFileIO", "\n?:  a1.txt", "\n?:  sub.d",
     "\n?:  .a4.hidden", "\n?:  a2", "\n?:  subnew", "\n?:  a3"]

res_1a =   -- remove special dir entry . and ..
    ["\nD:  /home/frank/testFileIO"
    , "\n?:  ."
    , "\n?:  a1.txt"
    , "\n?:  sub.d"
    , "\n?:  .."
    , "\n?:  .a4.hidden"
    , "\n?:  a2"
    , "\n?:  subnew"
    , "\n?:  a3"]


