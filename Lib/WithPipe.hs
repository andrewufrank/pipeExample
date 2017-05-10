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
--import  Pipes
import qualified Pipes.Prelude as Pipe
--import  Pipes.Prelude
import Pipes ((>->), (~>))

import qualified System.Directory as D
import qualified System.Posix as Posix
import   System.FilePath.Posix  ((</>))
import Lib.NoPipe (processOneDirEntry, directoryContent, processOneFile
                        , testDir, resTestDir6)
import System.IO


startPipe :: FilePath -> FilePath ->  IO ()
startPipe target store = do
    putStrLn ("start pipe " ++ target)
    res <- startPipe2 target store
    return ()

startPipe2 :: FilePath -> FilePath -> IO ()
-- ^ collect the filenames and md5
startPipe2 target store = do
    storeHandle <- openFile store WriteMode
    Pipe.runEffect $  effect target storeHandle
    hClose storeHandle
    return  ()


effect :: FilePath -> Handle -> Pipe.Effect IO ()  -- useful to fix types
effect fp storeHandle =  Pipe.for (initialProducer fp) $ \dir ->
                recurseDir dir
                >-> Pipe.toHandle storeHandle
--                >-> finalConsumer
--                >-> Pipe.toListM

initialProducer :: FilePath -> Pipe.Producer String IO ()
initialProducer fp = do
    Pipe.yield fp

finalConsumer ::  Pipe.Consumer String IO ()
finalConsumer = do
    st <- Pipe.await
    Pipe.lift $ putStrLn st
    finalConsumer


--recurseDir :: FilePath -> Pipe FilePath String IO ()
-- must not have a defined type
recurseDir fp = do
    filex <- Pipe.lift $ D.doesFileExist fp
    direx <- Pipe.lift $ D.doesDirectoryExist fp
    if filex
--    stat <- Pipe.lift $ Posix.getFileStatus fp
--    let isRegular = Posix.isRegularFile stat
--    if isRegular
        then do
            Pipe.lift $ putStrLn ("isFile " ++ fp )
            res <- Pipe.lift $ processOneFile fp
            Pipe.yield res
        else if direx then  do  -- add more tests!
            Pipe.lift $ putStrLn ("isDir " ++ fp)
--            let res1 = unwords ["\nD: ", fp]
            res1 <- Pipe.lift $ processOneDirEntry fp
            Pipe.yield res1
            content ::[FilePath]  <- Pipe.lift $ directoryContent fp
--            lift $ putStrLn . show $ content
            Prelude.mapM_ recurseDir content  --   recursion (mapM from Prelude, not Pipe!)
        else return ()
    return ()

-- isFile /home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG
--pipeExample: /home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG: hGetBufSome: hardware fault (Input/output error)

----------------------------------------------support code (not pipes)
--processFile1 :: FilePath -> IO String
---- ^ process one file - print filename as a stub
--processFile1 fn = do
--    md <-  getMD5 fn
--    let res = unwords ["F:", fn, show md]
--    return res

----processDir1 :: FilePath -> IO [FilePath ]
---- ^ process one directory
--processDir1 dir = do
--    content <- D.getDirectoryContents dir
--    -- exclude special files:
--    let content2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) content
--    -- complete the names to absolute filepath
--    let content3 = map (dir </>) content2
--    -- process each entry
--    return content3


--test_1 = do
--    putStrLn "----------------------------"
--    startPipe testDir
--    putStrLn "============================"
--    assertBool False  -- to force output


