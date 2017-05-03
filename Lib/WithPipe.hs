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

import Pipes
import System.IO as S
import Control.Monad (unless)

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import qualified Pipes.Prelude as P
import Pipes ((>->), (~>), Pipe (..))

import qualified System.Directory as D
import qualified System.Posix as Posix
import   System.FilePath.Posix  ((</>))
--import Safe
--import qualified Data.ByteString.Lazy  as L
import Lib.NoPipe (getMD5, res_6a)
--import Data.Traversable

startPipe :: FilePath ->  IO String
-- ^ collect the filenames and md5
startPipe fp = do
    res <- Pipe.runEffect $  effect9
    return "test" -- res


effect8 :: Effect IO ()
effect8 =  (initialProducer "/home/frank/Workspace8/testDirFileIO")
                >-> processOneDirEntry0
                >-> finalConsumer0

effect9 :: Effect IO ()
effect9 =  for (initialProducer "/home/frank/Workspace8/testDirFileIO") $ \dir ->
                processOneDirEntry dir
                >-> finalConsumer0

--initialProducer :: FilePath -> Pipe.Producer String IO ()
initialProducer fp = do
    Pipe.yield fp
--    return ()


--finalConsumer :: String -> Consumer String IO ()
finalConsumer0 = do
    st <- await
    Pipe.lift $ putStrLn st
    finalConsumer0
--    return (a:: _)

--putStrConsumer :: String -> Consumer String IO ()
putStrConsumer st =  lift $ putStrLn st


--processOneDirEntry :: FilePath -> IO [String]
--processOneDirEntry :: FilePath -> Pipe FilePath String IO ()
processOneDirEntry0 ::  Pipe FilePath String IO ()
processOneDirEntry0   = do
    fp <- Pipe.await
    Pipe.yield ("start " ++ fp)
    stat <- Pipe.lift $ Posix.getFileStatus fp
    let isRegular = Posix.isRegularFile stat
    if isRegular
        then do
            res <- Pipe.lift $ processFile1 fp
            Pipe.yield res
        else do
            Pipe.yield ("startDir " ++ fp)
            content ::[FilePath]  <- Pipe.lift $ processDir1 fp
            lift $ putStrLn . show $ content
            Pipe.each content -- processOneDirEntry
        --             processDir fp
--            Pipe.yield ("dir " ++ fp)
    processOneDirEntry0
    return ()

--processOneDirEntry :: FilePath -> IO [String]
--processOneDirEntry :: FilePath -> Pipe FilePath String IO ()
--processOneDirEntry ::  Pipe FilePath String IO ()
processOneDirEntry fp = do
--    fp <- Pipe.await
    stat <- Pipe.lift $ Posix.getFileStatus fp
    let isRegular = Posix.isRegularFile stat
    if isRegular
        then do
            res <- Pipe.lift $ processFile1 fp
            Pipe.yield res
        else do
            Pipe.yield ("startDir " ++ fp)
            content ::[FilePath]  <- Pipe.lift $ processDir1 fp
            lift $ putStrLn . show $ content
            mapM_ processOneDirEntry content
--    processOneDirEntry
    return ()


----processDir :: FilePath -> IO [String]
---- ^ process one directory
--processDir ::  FilePath ->    Pipe.Pipe String FilePath IO ()
--processDir dir = do
----    putStrLn . unwords $ ["processDirectory - ", dir]
----    dir <- Pipe.await
--    content ::[FilePath]  <- Pipe.lift $ processDir1 dir
--
--    Pipe.each content -- processOneDirEntry
--
--    return () -- yRes

----------------------------------------------support code (not pipes)
processFile1 :: FilePath -> IO String
-- ^ process one file - print filename as a stub
processFile1 fn = do
--    putStrLn . unwords $ ["processFile - ", fn]
--    s <- readFile fn
    md <-  getMD5 fn
    let res = unwords ["\nF:", fn, show md]
    return res

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


test_1 = do
    putStrLn "----------------------------"
--    res <- processDir "/home/frank/Workspace8/testDirFileIO"
    startPipe "/home/frank/Workspace8/testDirFileIO"
    putStrLn "============================"
    assertBool False  -- to force output


------- old examples
effect1 :: Effect IO ()
effect1 = for stdinLn $ \str ->
                lift $ putStrLn str

effect2 :: Effect IO ()
effect2 = for (initialProducer "/home/frank/Workspace8/testDirFileIO") $ \dir ->
                lift $ putStrLn dir

effect3 :: Effect IO ()
effect3 = for stdinLn $ \str ->
                finalConsumer str

effect4 :: Effect IO ()
effect4 = for stdinLn $ \str ->
                putStrConsumer str

effect5 :: Effect IO ()
effect5 =  for stdinLn
                finalConsumer

------effect6 :: Effect IO ()
--effect6 =   P.stdinLn  >-> Pipe.toListM  - possibly problem with toListM

effect7 :: Effect IO ()
effect7 =  (initialProducer "/home/frank/Workspace8/testDirFileIO")
                >-> P.stdoutLn


tut1 :: Effect IO ()
tut1 =  P.stdinLn >-> P.stdoutLn
--stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift S.isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop




--finalConsumer :: String -> Consumer String IO ()
finalConsumer st = do
    Pipe.lift $ putStrLn st
--    return (a:: _)

