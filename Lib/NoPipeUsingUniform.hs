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
--import Uniform.FileIO  -- replaced by:
import Path
import Path.IO
import Control.Monad.Catch

--
import qualified Data.ByteString.Lazy  as L
import           Data.Digest.Pure.MD5  (md5)
import qualified System.Directory      as S
import Control.DeepSeq (($!!), force)
import qualified System.Posix          as P

--
import Uniform.FileStatus
import Uniform.Strings hiding ((</>))

import Control.Monad
import Data.Either

--instance CharChains (Path a d) where toString  = show
instance CharChains2 (Path a d) String where show'  = show
instance CharChains2 (Path a d) Text where show'  = s2t . show

recurseDirUU :: Path Abs Dir -> IO (ErrOrVal [Text])
recurseDirUU fp = runErr $ processDir fp

--recurseDir :: Path Abs f -> ErrIO [String]
---- main entry point
--recurseDir fp = do
--    putIOwords ["recurseDir", showT fp]
--    fx <- doesFileExist fp
--    dx <- doesDirExist fp
--    if fx then do
--                        r <- processOneFile fp
--                        return [r]
--        else if dx then  processDir fp
--                    else do
--                            putIOwords ["recurse neither file nor dir", showT fp]
--                            return []

--    stat <- getFileStatus' fp
--    res1 <- if isRegularFile stat
--        then do
--            r <- processOneFile fp
--            return [r]
--        else do
--            processDir fp
--    return res1

processDir :: Path Abs Dir -> ErrIO [Text]
-- ^ process one directory
processDir dir = do
    -- process the dir as an entry
    res1 <- processOneDirEntry dir
    (dirs, files) <- listDir dir
    res3 <- mapM processDir dirs
    res4 <- mapM processOneFile files
    return $ concat [res1,  concat res3,  concat res4]

processOneDirEntry :: Path Abs Dir -> ErrIO [Text]
processOneDirEntry dir = do
    let res1 =  unwords' ["\nD: ", show'  dir]
    return [res1]

--directoryContent  :: Path Abs Dir -> ErrIO [String]
---- ^ find the entries in a directory - no recursion yet
--directoryContent dir = do
--    content :: [FilePath]  <- getDirCont dir
----    putIOwords ["directoryContent - ", s2t dir, showT content]
--    return content

processOneFile :: Path Abs File -> ErrIO [Text]
-- ^ process one file - print filename as a stub
processOneFile fn = do
    perm <- getPermissions fn
    if readable perm
        then do
--                putIOwords ["processOneFile test ", showT fn, "readable", showT isReadable]
                md <- getMD5 fn
                let res = unwords' ["\nF:", showT fn, showT md]
--                putIOwords ["processOneFile done ", showT fn, "readable", showT isReadable]
                return [res]
            `catchError` \(e :: Text)  -> do
                putIOwords ["processOneFile error ", showT fn, "readable", showT perm, "\n", showT e]
--                throwErrorT ["processOneFile - problem with getMD5", showT e]
                -- could be simply
                return $ [ unwords' ["\nF:", show' fn, ""]]
        else return []

--getMD5z fn = do
--    putIOwords ["a"]
--    md <- getMD5 fn
--    putIOwords ["b"]
--    return md

getMD5 :: Path Abs File -> ErrIO (Maybe Text)
getMD5 fn =
    callIO $ do
            filedata :: L.ByteString <- L.readFile (toFilePath fn)  -- fails for some special files eg. /proc
--                    putIOwords ["getMD5 in FileStrings.hs file 2"]
            let res = showT $ md5  filedata
--                    putIOwords ["getMD5 in FileStrings.hs file 3"]
            return $!! (Just res)

----            putIOwords ["getMD5 in FileStrings.hs", showT fn]
--            status <- getSymbolicLinkStatus fn
----            let status = fromJustNote "getMD5 xx33" $ mstatus
--            let regular = isRegularFile status
--            readable <- getFileAccess fn (True, False, False)
----            putIOwords ["getMD5 in FileStrings.hs before if"]
--            if regular && readable then callIO $ do
--    --                    putIOwords ["getMD5 in FileStrings.hs file 1"]
       `catch` \(e::SomeException) -> do
                putIOwords ["caught with catch in getmd5 ", showT e]
                return Nothing
--
--                else throwErrorT $ ["getMD5 error file not readable" , showT fn]

--        `catchError` \e -> do
--            putIOwords ["getMD5 in FileStrings.hs", showT fn, showT e]  -- reached
--            throwErrorT $ ["getMD5 error for" , showT fn]

--
--getDirCont fn  = do
----        putIOwords ["getDirCont", show f]
--        testDir <- doesDirExist fn
--        readExec <- getFileAccess fn (True, False, True)
--        if testDir && readExec then
--            do
--               r <- callIO . S.listDirectory $ fn
--               let r2 = filter ( \file' -> (file' /= "." && file' /= "..")  ) r
--               let r3 = map (fn </>) r2
----               putIOwords ["FileStrigs - getDirCont", showT fn, "files: ", unwordsT . map showT $ r]
--               return r3
--          else
--                throwErrorT
--                    ["getDirCont not exist or not readable"
--                    , showT fn, showT testDir, showT readExec]

--doesDirExist = callIO . S.doesDirectoryExist
--doesFileExist   = callIO . S.doesFileExist

--getFileAccess fp (r,w,e) =
--        do
----            putIOwords ["getFileAccess", show fp]
--            callIO $
--                (do
--
--                    P.fileAccess (unL fp) r w  e
--              `catchError` \e -> do
--                     putIOwords ["getFileAccess error", showT fp, s2t $ show e]
--                     return False )

--getSymbolicLinkStatusFP :: Path   -> ErrIO ( P.FileStatus)
-- ^ get status if exist (else Nothing)
--   is the status of the link, does not follow the link
--
getSymbolicLinkStatusFP  fp = do
----    putIOwords ["fileio getSymbolicLinkStatus", fp]
    st <- callIO $ P.getSymbolicLinkStatus fp
----    putIOwords ["fileio getSymbolicLinkStatus done", fp]
    return   st


-- TODO fileio
--getFileStatus' :: FilePath  -> ErrIO P.FileStatus
--getFileStatus' fp = liftIO $ P.getFileStatus   fp


testDir =  parseRelDir "testDirFileIO"  :: Maybe (Path Rel Dir)  -- relative path for test, gives relative path in output
testDirAbs = parseAbsDir "/home/frank/Workspace8/pipeExample/testDirFileIO" :: Maybe (Path Abs Dir)
-- TODO error
test_2 = do
    res <-   recurseDirUU (fromJustNote "testdirabs" testDirAbs)
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
