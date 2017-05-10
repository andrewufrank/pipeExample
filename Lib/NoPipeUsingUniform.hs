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
    , TypeFamilies
    , ScopedTypeVariables
    , OverloadedStrings
    #-}


module Lib.NoPipeUsingUniform where

import           Test.Framework

import Uniform.Error
import Uniform.FileIO
import Uniform.FileStatus
import Uniform.Strings hiding ((</>))

import Control.Monad
import Data.Either

recurseDirUU :: FilePath -> IO (ErrOrVal [String])
recurseDirUU fp = runErr $ recurseDir fp

recurseDir :: FilePath -> ErrIO [String]
-- main entry point
recurseDir fp = do
    putIOwords ["recurseDir", s2t fp]
    stat <- getFileStatus' fp
    res1 <- if isRegularFile stat
        then do
            r <- processOneFile fp
            return [r]
        else do
            processDir fp
    return res1

processDir :: FilePath -> ErrIO [String]
-- ^ process one directory
processDir dir = do
    -- process the dir as an entry
    res1 <- processOneDirEntry dir
    content3 <- directoryContent dir
    res3 <- mapM recurseDir content3
    return (res1 : concat res3)

processOneDirEntry :: FilePath -> ErrIO String
processOneDirEntry dir = do
    let res1 = unwords ["\nD: ", dir]
    return res1

directoryContent  :: FilePath -> ErrIO [String]
-- ^ find the entries in a directory - no recursion yet
directoryContent dir = do
    content :: [FilePath]  <- getDirCont dir
--    putIOwords ["directoryContent - ", s2t dir, showT content]
    return content

processOneFile :: FilePath -> ErrIO String
-- ^ process one file - print filename as a stub
processOneFile fn = do
    isReadable <- getFileAccess fn (True, False, False)
    if isReadable
        then do
--                putIOwords ["processOneFile test ", showT fn, "readable", showT isReadable]
                md <- getMD5 fn
                let res = unwords ["\nF:", fn, show md]
--                putIOwords ["processOneFile done ", showT fn, "readable", showT isReadable]
                return res
            `catchError` \(e :: Text)  -> do
                putIOwords ["processOneFile error ", showT fn, "readable", showT isReadable, "\n", showT e]
--                throwErrorT ["processOneFile - problem with getMD5", showT e]
                -- could be simply
                return $ unwords ["\nF:", fn, ""]
        else return ""

getMD5z fn = do
    putIOwords ["a"]
    md <- getMD5 fn
    putIOwords ["b"]
    return md


-- TODO fileio
--getFileStatus' :: FilePath  -> ErrIO P.FileStatus
--getFileStatus' fp = liftIO $ P.getFileStatus   fp


testDir = "testDirFileIO" :: FilePath  -- relative path for test, gives relative path in output
testDirAbs = "/home/frank/Workspace8/pipeExample/testDirFileIO" ::FilePath
-- TODO error
test_2 = do
    res <- runErr $ recurseDir testDir
    assertEqual (Right resTestDir6) res

resTestDir6 =
     ["\nD:  testDirFileIO",
   "\nF: testDirFileIO/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
   "\nD:  testDirFileIO/sub.d",
   "\nF: testDirFileIO/sub.d/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
   "\nD:  testDirFileIO/sub.d/.hiddensub.d",
   "\nF: testDirFileIO/sub.d/.hiddensub.d/a1.txt Just \"562fade7e712814aec485852d3f5f6dc\"",
   "\nF: testDirFileIO/sub.d/.hiddensub.d/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\"",
   "\nF: testDirFileIO/sub.d/.hiddensub.d/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
   "\nF: testDirFileIO/sub.d/.hiddensub.d/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
   "\nF: testDirFileIO/sub.d/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\"",
   "\nF: testDirFileIO/sub.d/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
   "\nF: testDirFileIO/sub.d/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
   "\nF: testDirFileIO/.a4.hidden Just \"a6f26e70990ed9c122288bfea23e2060\"",
   "\nF: testDirFileIO/a2 Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
   "\nD:  testDirFileIO/subnew",
   "\nF: testDirFileIO/a3 Just \"9d607a663f3e9b0a90c3c8d4426640dc\""]

--this file is corrupt
test_jpgNoPipe = do
    res <- runErr$ recurseDir "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG"
    -- just gives no md5 value
    assertBool (isRight res)   -- oporto has corrupt file
--    assertEqual (Right ["\nF: /home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG "]) res
