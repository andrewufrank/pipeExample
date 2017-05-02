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

import qualified System.Directory as D
import qualified System.Posix as P
import   System.FilePath.Posix  ((</>))

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
    let md = 0
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
    res <- processDir "/home/frank/testFileIO"
    assertEqual res_4a res

res_4a =
    ["\nD:  /home/frank/testFileIO",
     "\nF: /home/frank/testFileIO/a1.txt 0",
     "\nD:  /home/frank/testFileIO/sub.d",
     "\nF: /home/frank/testFileIO/sub.d/a1.txt 0",
     "\nD:  /home/frank/testFileIO/sub.d/.hiddensub.d",
     "\nF: /home/frank/testFileIO/sub.d/.hiddensub.d/a1.txt 0",
     "\nF: /home/frank/testFileIO/sub.d/.hiddensub.d/.a4.hidden 0",
     "\nF: /home/frank/testFileIO/sub.d/.hiddensub.d/a2 0",
     "\nF: /home/frank/testFileIO/sub.d/.hiddensub.d/a3 0",
     "\nF: /home/frank/testFileIO/sub.d/.a4.hidden 0",
     "\nF: /home/frank/testFileIO/sub.d/a2 0",
     "\nF: /home/frank/testFileIO/sub.d/a3 0",
     "\nF: /home/frank/testFileIO/.a4.hidden 0",
     "\nF: /home/frank/testFileIO/a2 0",
     "\nD:  /home/frank/testFileIO/subnew",
     "\nF: /home/frank/testFileIO/a3 0"]

res_3a =
    ["\nD:  /home/frank/testFileIO",
     "\n?:  /home/frank/testFileIO/.",
     "\n?:  /home/frank/testFileIO/a1.txt",
     "\n?:  /home/frank/testFileIO/sub.d",
     "\n?:  /home/frank/testFileIO/..",
     "\n?:  /home/frank/testFileIO/.a4.hidden",
     "\n?:  /home/frank/testFileIO/a2",
     "\n?:  /home/frank/testFileIO/subnew",
     "\n?:  /home/frank/testFileIO/a3"]

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


