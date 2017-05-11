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
--import  Pipes
import qualified Pipes.Prelude as Pipe
--import  Pipes.Prelude
import Pipes ((>->), (~>))

--import qualified System.Directory as D
--import qualified System.Posix as Posix
--import   System.FilePath.Posix  ((</>))
--import Lib.NoPipe (processOneDirEntry, directoryContent, processOneFile
--                        , testDir, resTestDir6)

import Uniform.Error
import Path         as Path
import Path.IO  as PIO
import System.IO as S
import System.Directory as D
import System.FilePath.Posix (dropTrailingPathSeparator)
--import System.Posix as Posix

--import Uniform.FileIO
import Uniform.FileStatus
import Uniform.Strings hiding ((</>))

--import Lib.NoPipeUsingUniform (getMD5, testDir )
import Lib.NoPipe  (getMD5)

import Data.Maybe
import Data.List (sort)
import Control.Exception

instance CharChains2 (Path a d) String where show'  = show
instance CharChains2 (Path a d) Text where show'  = s2t . show

startPipe :: Path Abs Dir  -> Path Rel File ->  IO ()
startPipe target store = do
    res <- runErr $ startPipe2 target store
    putIOwords ["startPipe result", showT res]
    return ()

startPipe2 :: Path Abs Dir  -> Path Rel File ->ErrIO ()
-- ^ collect the filenames and md5
startPipe2 target store = do
    storeHandle <- callIO $ openFile (toFilePath store) WriteMode  -- missing in path
    callIO $ Pipe.runEffect $  effect target storeHandle
    callIO $ S.hClose  storeHandle
    return  ()

effect :: Path Abs Dir -> Handle -> Pipe.Effect IO ()  -- useful to fix types
effect target storeHandle =  Pipe.for (initialProducer target) $ \dir ->
                recurseDir dir
                >-> Pipe.toHandle storeHandle
--                >-> finalConsumer
--                >-> Pipe.toListM

initialProducer :: Path Abs Dir -> Pipe.Producer (Path Abs Dir) IO ()
initialProducer fp = do
    Pipe.yield fp

--finalConsumer ::  Pipe.Consumer String ErrIO ()
--finalConsumer = do
--    st <- Pipe.await
--    Pipe.lift $ callIO $  putStrLn st
--    finalConsumer

--processOneDirEntry :: Path Abs Dir ->  IO Text
--processOneDirEntry dir = do
--    let res1 =  unwords' ["\nD: ", show'  dir]
--    return  res1

----processOneFileP :: Path Abs File -> ErrIO [Text]
---- ^ process one file - print filename as a stub
--processOneFileP fn = do
--    res <- Pipe.lift $ processOneFile fn
--    case res of
--        Nothing -> return ()
--        Just t -> Pipe.yield t


--processOneFile :: Path Abs File -> Pipe.Pipe (Pipe.X) String ErrIO ()
-- ^ process one file - print filename as a stub
processOneFile fn = do
    putIOwords ["processOneFile test ", showT fn]
    fx <- PIO.doesFileExist fn
    when fx $ do
        perm <-Pipe.lift $ PIO.getPermissions fn
        res <- if readable perm
            then  do
    --                putIOwords ["processOneFile test ", showT fn, "readable", showT isReadable]
                    md <- Pipe.lift $ getMD5 (toFilePath fn)
                    let res = unwords' ["\nF:", showT fn, showT md]
    --                putIOwords ["processOneFile done ", showT fn, "readable", showT isReadable]
                    return (Just res)
    --            `catch` \(e :: SomeException)  -> do
    --                putIOwords ["processOneFile error ", showT fn, "readable", showT perm, "\n", showT e]
    --                throwErrorT ["processOneFile - problem with getMD5", showT e]
                    -- could be simply
                    return . Just . unwords' $ ["\nF:", show' fn, ""]
            else return Nothing
        Pipe.yield (maybe "" t2s res)
        return () -- (show res)

xisSymbolicLink t =  D.pathIsSymbolicLink   (dropTrailingPathSeparator $ toFilePath t)
--recurseDir :: Path Abs Dir  -> Pipe.Pipe (Pipe.X) String ErrIO ()
-- must not have a defined type
recurseDir fp = do
    putIOwords ["recurseDir start", showT fp]  -- not effective
    perm <-Pipe.lift $ PIO.getPermissions fp
    when (readable perm && searchable perm) $ do
            symLink <- Pipe.lift $ xisSymbolicLink fp
            if symLink
                then do
                        putIOwords ["recurseDir symlink", showT fp]
                        return ()
                else do
                    let res1 = unwords' ["\nD: ", show'  fp]
            --         res1 :: Text <- Pipe.lift $ processOneDirEntry fp  -- callIO just to get error
                    Pipe.yield res1
                    (dirs, files) <- Pipe.lift $ listDir  fp
                    Prelude.mapM_ processOneFile (sort files)
                    Prelude.mapM_ recurseDir (sort dirs)
                    return ()

--        return ()
----    `catch` \(e::SomeException) -> do
----            putIOwords ["recurseDir - error catch: ", showT e]
----            return ()

resFile0 = fromJustNote "testdir1" $ parseRelFile "result20"
resFileN = fromJustNote "testdir2" $ parseRelFile "result2N" :: Path Rel File
testDir = fromJustNote "testdir3" $ parseAbsDir "/home/frank/Workspace8/pipeExample/testDirFileIO" :: Path Abs Dir
test_PUU :: IO ()
test_PUU = do
    putStrLn "----------------------------"
    startPipe testDir resFileN
    putStrLn "============================"
    r0 <- readFile (toFilePath resFile0)
    rN <- readFile (toFilePath resFileN)
    assertEqual r0 rN

mkFilenameRelFile fn = fromJustNote ("mkFilenameRelFile " ++ fn) $ parseRelFile fn
mkFilenameAbsDir fn = fromJustNote ("mkFilenameAbsDir " ++ fn) $ parseAbsDir fn

pFile0 = mkFilenameRelFile "presult0"
pFileN = mkFilenameRelFile "presultN"
--testPhotos = mkFilenameAbsDir "/home/frank/additionalSpace/Photos_2016/"
testPhotos = mkFilenameAbsDir "/home/frank/additionalSpace/Photos_2016/sizilien2016"
readFile3 fn = readFile (toFilePath fn)

--test_Photos :: IO ()
--test_Photos = do
--    putStrLn "----------------------------"
--    startPipe testPhotos pFileN
--    putStrLn "============================"
--    r0 <- readFile3 pFile0
--    rN <- readFile3 pFileN
--    assertEqual r0 rN

hFile0 = mkFilenameRelFile "hresult0"
hFileN = mkFilenameRelFile "hresultN"
homeDir = mkFilenameAbsDir "/home/frank"

test_home :: IO ()
test_home = do
    putStrLn "----------------------------"
    startPipe homeDir hFileN
    putStrLn "============================"
    r0 <- readFile3 hFile0
    rN <- readFile3 hFileN
    assertEqual r0 rN

test_symlink :: IO ()
test_symlink = do
    let t =   mkFilenameAbsDir "/bin/X11/X11"
    isSymlink1 <- D.pathIsSymbolicLink   (dropTrailingPathSeparator $ toFilePath t)
    isSymlink2 <- D.pathIsSymbolicLink "/bin/X11/X11" -- (toFilePath t)
    isSymlink3 <- D.pathIsSymbolicLink "/bin/X11/X11/" -- (toFilePath t)
    assertEqual  (True, True, False) (isSymlink1, isSymlink2, isSymlink3)

--test_toFP :: IO ()
--test_toFP = do
--    let t =   mkFilenameAbsDir "/bin/X11/X11"
--    let t2 = toFilePath t
--    assertEqual "/bin/X11/X11" t2

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
--
---- this file is corrupt
----test_jpg = do
----    res <- startPipe "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG" "testjpg"
----    assertEqual () res
--
--test_1 = do
--    res <- startPipe "/usr/share/lyx" "res_test1"
--    assertEqual () res


