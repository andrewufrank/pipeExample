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
    , PackageImports
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

--import Uniform.Error
--import qualified Path         as Path
--import Path.IO  as PIO
--import System.IO as S
--import System.Directory as D
--import System.FilePath.Posix (dropTrailingPathSeparator)
--import System.Posix as Posix

--import "monads-tf" Control.Monad.Error as CME

import Uniform.FileIOalgebra
import Uniform.FileStrings
import Uniform.FileStatus
import Uniform.Error
import Uniform.Strings hiding ((</>))

--import Lib.NoPipeUsingUniform (getMD5, testDir )
--import Lib.NoPipe  (getMD5)

import Data.Maybe
import Data.List (sort)
import Control.Exception

startPipe :: Path Abs Dir  -> Path r File ->  IO ()
startPipe target store = do
    res <- runErr $ startPipe2 target store
    putIOwords ["startPipe result", showT res]
    return ()

startPipe2 :: Path Abs Dir  -> Path d File -> ErrIO ()
-- ^ collect the filenames and md5
startPipe2 target store = do
    storeHandle <-  openFile2handle store WriteMode  -- missing in path
    Pipe.runEffect $  effect target storeHandle
    closeFile2 storeHandle
    return  ()

--effect ::  (ErrorType ((Pipe.Proxy
--                                         Pipe.X
--                                         ()
--                                         ()
--                                         String
--                                         (CME.ErrorT
--                                            Text IO))) ~ Text) =>
--                Path Abs Dir -> Handle -> Pipe.Effect ErrIO ()  -- useful to fix types
effect target storeHandle = Pipe.for (initialProducer target)  $ \dir ->
--                initialProducer
                recurseDir dir
                >-> Pipe.toHandle storeHandle
--                >-> finalConsumer
--                >-> Pipe.toListM
--                    $ target

--initialProducer :: Path Abs Dir -> Pipe.Producer (Path Abs Dir) ErrIO ()
--initialProducer :: Path Abs Dir -> Pipe.Proxy (Pipe.X) (Pipe.X) ()  (Path Abs Dir) IO ()
initialProducer fp = do
    Pipe.yield fp

--finalConsumer ::  Pipe.Consumer String ErrIO ()
--finalConsumer = do
--    st <- Pipe.await
--    Pipe.lift $ callIO $  putStrLn st
--    finalConsumer


--processOneFile ::  -- (ErrorType (Pipe.Proxy x x' y y' ErrIO  ) ~ Text)
--          Path Abs File -> Pipe.Pipe (Pipe.X) String ErrIO ()
--processOneFile :: Path Abs File -> Pipe.Proxy (Pipe.X) (Pipe.X) ()  String IO ()
-- ^ process one file - print filename as a stub
processOneFile ::Path b File
                      -> Pipe.Proxy Pipe.X () () String (ErrorT Text IO) ()
processOneFile fn = do
--    putIOwords ["processOneFile test ", showT fn]
    fx <- Pipe.lift $  doesFileExist' fn
    if not fx
        then do
            Pipe.lift $ putIOwords ["processOneFile not exis ", showT fn]
            return ()
        else  do
            perm <-Pipe.lift $ getPermissions' fn
            res <- if readable perm
                then  do
        --                putIOwords ["processOneFile test ", showT fn, "readable", showT isReadable]
                        md <- Pipe.lift $   getMD5 (toFilePath fn)
                        let res = unwords' ["\nF:", showT fn, showT md]
        --                putIOwords ["processOneFile done ", showT fn, "readable", showT isReadable]
                        return (Just res)
        --            `catch` \(e :: SomeException)  -> do
        --                putIOwords ["processOneFile error ", showT fn, "readable", showT perm, "\n", showT e]
        --                throwErrorT ["processOneFile - problem with getMD5", showT e]
                        -- could be simply
--                        return . Just . unwords' $ ["\nF:", show' fn, ""]
                else do
                    Pipe.lift $ putIOwords ["processOneFile not readable ", showT fn]
                    return Nothing
            Pipe.yield (maybe "" t2s res)
            return () -- (show res)

--xisSymbolicLink t =  D.pathIsSymbolicLink   (dropTrailingPathSeparator $ toFilePath t)

--recurseDir :: Path Abs Dir  -> Pipe.Pipe (Pipe.X) String IO ()
--recurseDir ::  (ErrorType  (Pipe.Proxy (Pipe.X) (Pipe.X) ()  String ErrIO) ~ Text) =>
--    Path Abs Dir  -> Pipe.Proxy (Pipe.X) (Pipe.X) ()  String ErrIO ()
-- must not have a defined type
recurseDir :: Path Abs Dir
                      -> Pipe.Proxy Pipe.X () () String (ErrorT Text IO) ()
recurseDir fp = do
--    putIOwords ["recurseDir start", showT fp]
    perm <-Pipe.lift $ getPermissions' fp
    if not (readable perm && searchable perm)
        then do
            Pipe.lift $ putIOwords ["recurseDir not readable or not searchable", showT fp]
--            throwError ("something" :: Text)
            return ()
        else do
            symLink <- Pipe.lift $ checkSymbolicLink fp -- callIO $ xisSymbolicLink fp
            if symLink
                then do
                        Pipe.lift $ putIOwords ["recurseDir symlink", showT fp]
                        return ()
                else do
                    let res1 = unwords' ["\nD: ", show'  fp]
            --         res1 :: Text <- Pipe.lift $ processOneDirEntry fp  -- callIO just to get error
                    Pipe.yield res1
                    (dirs, files) <- Pipe.lift   $ listDir  fp
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

--mkFilenameRelFile fn = fromJustNote ("mkFilenameRelFile " ++ fn) $ parseRelFile fn
--makeAbsFile fn = fromJustNote ("mkFilenameRelFile " ++ fn) $ parseAbsFile fn
--mkFilenameAbsDir fn = fromJustNote ("mkFilenameAbsDir " ++ fn) $ parseAbsDir fn

pFile0 = makeAbsFile "/home/frank/presult0"
pFileN = makeAbsFile "/home/frank/presultN"
--testPhotos = mkFilenameAbsDir "/home/frank/additionalSpace/Photos_2016/"
testPhotos = makeAbsDir "/home/frank/additionalSpace/Photos_2016/sizilien2016"
readFile3 fn = readFile (toFilePath fn)

--test_Photos :: IO ()
--test_Photos = do
--    putStrLn "----------------------------"
--    startPipe testPhotos pFileN
--    putStrLn "============================"
--    r0 <- readFile3 pFile0
--    rN <- readFile3 pFileN
--    assertEqual r0 rN

hFile0 = makeAbsFile "/home/frank/hresult0"
hFileN = makeAbsFile "/home/frank/hresultN"
homeDir = makeAbsDir "/home/frank"

--test_home :: IO ()
--test_home = do
--    putStrLn "----------------------------"
--    startPipe homeDir hFileN
--    putStrLn "============================"
--    r0 <- readFile3 hFile0
--    rN <- readFile3 hFileN
--    assertEqual r0 rN


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


