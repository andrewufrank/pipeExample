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
    , OverloadedStrings
    #-}


module Lib.PipeUsingUniform where

import           Test.Framework

import qualified Pipes as Pipe
import qualified Pipes.Prelude as Pipe
import Pipes ((>->), (~>))


import Uniform.Error
import Uniform.FileIO
import Uniform.FileStatus
import Uniform.Strings hiding ((</>))

import Lib.NoPipeUsingUniform (processOneFile, directoryContent, processOneDirEntry
                    , testDir )

import Data.Maybe
import Control.Exception

startPipe :: FilePath -> FilePath ->  IO ()
startPipe target store = do
    res <- runErr $ startPipe2 target store
    putIOwords ["startPipe result", showT res]
    return ()

startPipe2 :: FilePath -> FilePath ->ErrIO ()
-- ^ collect the filenames and md5
startPipe2 target store = do
    storeHandle <- openFile store WriteMode
    Pipe.runEffect $  effect target storeHandle
    closeFile2 storeHandle
    return  ()

effect :: FilePath -> Handle -> Pipe.Effect ErrIO ()  -- useful to fix types
effect target storeHandle =  Pipe.for (initialProducer target) $ \dir ->
                recurseDir dir
                >-> Pipe.toHandle storeHandle

initialProducer :: FilePath -> Pipe.Producer String ErrIO ()
initialProducer fp = do
    Pipe.yield fp

finalConsumer ::  Pipe.Consumer String ErrIO ()
finalConsumer = do
    st <- Pipe.await
    Pipe.lift $ callIO $  putStrLn st
    finalConsumer


--recurseDir :: FilePath -> Pipe FilePath String ErrIO ()
-- must not have a defined type
recurseDir fp = do
    stat <- Pipe.lift $ Uniform.FileStatus.getSymbolicLinkStatusFP (fp :: FilePath)
    if isSymbolicLink stat
        then  do
--                putIOwords ["recurseDir - symbolid link dropped", s2t fp]
                return ()   -- avoid symbolic links immediately, lead to infinite loops
                            -- and can be broken
        else  do
            isReadExecutable <- Pipe.lift $ getFileAccess fp (True, False, True)
            isReadable <- Pipe.lift $ getFileAccess fp (True, False, False)
            if not isReadable then  do
--                    putIOwords ["recurseDir - not readable", s2t fp]
                    return ()  --   recursion (mapM from Prelude, not Pipe!)
               else
    --            putIOwords ["recureseDir is readable", showT fp]
                 if isRegularFile stat
                            then do
                                res <- Pipe.lift $ processOneFile fp
    --                            putIOwords ["recureseDir 1 file", showT fp]
                                Pipe.yield res
    --                            putIOwords ["recureseDir  2 file", showT fp]
                            else if isDirectory stat && isReadExecutable
                                    then do  -- add more tests!
                                            res1 <- Pipe.lift $ processOneDirEntry fp
                                            Pipe.yield res1
                                            content ::[FilePath]  <- Pipe.lift $ directoryContent fp
            --                              lift $ putStrLn . show $ content
                                            Prelude.mapM_ recurseDir content
                                    else do
--                                        putIOwords ["recurseDir - not readexe, not regular file", s2t fp]
                                        return ()  --   recursion (mapM from Prelude, not Pipe!)
    return ()
--    `catch` \(e::SomeException) -> do
--            putIOwords ["recurseDir - error catch: ", showT e]
--            return ()

resFile0 = "result0" :: FilePath
resFileN = "resultN" :: FilePath

--test_PUU :: IO ()
--test_PUU = do
--    putStrLn "----------------------------"
--    startPipe testDir resFileN
--    putStrLn "============================"
--    r0 <- readFile resFile0
--    rN <- readFile resFileN
--    assertEqual r0 rN

pFile0 = "presult0" :: FilePath
pFileN = "presultN" :: FilePath
testPhotos = "/home/frank/additionalSpace/Photos_2016/" ::FilePath

test_Photos :: IO ()
test_Photos = do
    putStrLn "----------------------------"
    startPipe testPhotos pFileN
    putStrLn "============================"
    r0 <- readFile pFile0
    rN <- readFile pFileN
    assertEqual r0 rN

---- there are some special files (all of /proc?) which are read permission
---- but cannot be read
--test_maps = do
--    res <- runErr $ do
--        putIOwords ["tets_maps - trying to read file /proc/1/task/1/maps"]
--        let fn = "/proc/1/task/1/maps" :: FilePath
--        status <- getSymbolicLinkStatus fn
--        putIOwords ["status is found "]
----        let status = fromJustNote "test_maps" mstatus
--        readable <-  getFileAccess  fn $ (True, False, False)
--        putIOwords ["tets_maps - status"
--                , "regular",  showT . isRegularFile $ status
--                , "readable", showT readable
--                ]
--        res1 :: Text  <-  readFile2 fn
--        putIOwords ["tets_maps - result", showT res1]
--    assertEqual  (Left   "/proc/1/task/1/maps: openFile: permission denied (Permission denied)") res

---- this file is corrupt on oporto, but not on santafe
test_jpg = do
    res <- startPipe "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG" "testjpg"
    assertEqual () res

test_1 = do
    res <- startPipe "/usr/share/lyx" "res_test1"
    assertEqual () res


