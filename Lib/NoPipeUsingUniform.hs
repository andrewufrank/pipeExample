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
import Data.List

instance CharChains2 (Path a d) String where show'  = show
instance CharChains2 (Path a d) Text where show'  = s2t . show

recurseDirUU :: Path Abs Dir -> IO (ErrOrVal [Text])
recurseDirUU fp = runErr $ processDir fp

processDir :: Path Abs Dir -> ErrIO [Text]
-- ^ process one directory
processDir dir = do
    if (("/proc"::String) `isPrefixOf'` (toFilePath dir))
     then return []
     else do
        -- process the dir as an entry
        perms <- getPermissions dir
        if searchable perms
            then  do
                    res1 <- processOneDirEntry dir  -- callIO just to get error
                    (dirs, files) <- listDir  dir
                    res3 <- mapM processDir (sort dirs)
                    res4 <- mapM processOneFile (sort files)
                    return $ concat [res1,  concat res3,  concat res4]
               `catch` \(e::SomeException) -> do
                        unless (("/proc"::String) `isPrefixOf'` (toFilePath dir)) $
                           putIOwords ["caught with catch in procesDir ", showT e]
                        return []
            else return []

processOneDirEntry :: Path Abs Dir -> ErrIO [Text]
processOneDirEntry dir = do
    let res1 =  unwords' ["\nD: ", show'  dir]
    return [res1]

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


getMD5 :: Path Abs File -> ErrIO (Maybe Text)
getMD5 fn =
    callIO $ do
            filedata :: L.ByteString <- L.readFile (toFilePath fn)  -- fails for some special files eg. /proc
--                    putIOwords ["getMD5 in FileStrings.hs file 2"]
            let res = showT $ md5  filedata
--                    putIOwords ["getMD5 in FileStrings.hs file 3"]
            return $!! (Just res)
           `catch` \(e::SomeException) -> do
                    putIOwords ["caught with catch in getmd5 ", showT e]
                    return Nothing
--
--                else throwErrorT $ ["getMD5 error file not readable" , showT fn]

--        `catchError` \e -> do
--            putIOwords ["getMD5 in FileStrings.hs", showT fn, showT e]  -- reached
--            throwErrorT $ ["getMD5 error for" , showT fn]


testDir =  parseRelDir "testDirFileIO"  :: Maybe (Path Rel Dir)  -- relative path for test, gives relative path in output
testDirAbs = parseAbsDir "/home/frank/Workspace8/pipeExample/testDirFileIO" :: Maybe (Path Abs Dir)
-- TODO error
test_2 = do
    res <-   recurseDirUU (fromJustNote "testdirabs" testDirAbs)
    assertEqual (Right resTestDir6) res

resTestDir6 =
    ["\nD:  \"/home/frank/Workspace8/pipeExample/testDirFileIO/\"",
       "\nD:  \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/\"",
       "\nD:  \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.hiddensub.d/\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.hiddensub.d/.a4.hidden\" Just \"a6f26e70990ed9c122288bfea23e2060\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.hiddensub.d/a1.txt\" Just \"562fade7e712814aec485852d3f5f6dc\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.hiddensub.d/a2\" Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.hiddensub.d/a3\" Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/.a4.hidden\" Just \"a6f26e70990ed9c122288bfea23e2060\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/a1.txt\" Just \"562fade7e712814aec485852d3f5f6dc\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/a2\" Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/sub.d/a3\" Just \"9d607a663f3e9b0a90c3c8d4426640dc\"",
       "\nD:  \"/home/frank/Workspace8/pipeExample/testDirFileIO/subnew/\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/.a4.hidden\" Just \"a6f26e70990ed9c122288bfea23e2060\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/a1.txt\" Just \"562fade7e712814aec485852d3f5f6dc\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/a2\" Just \"a19e4fec5422bdf818f3b4ec8903d644\"",
       "\nF: \"/home/frank/Workspace8/pipeExample/testDirFileIO/a3\" Just \"9d607a663f3e9b0a90c3c8d4426640dc\""]


corruptFile = parseAbsFile "/home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG"

--this file is corrupt just gives no md5 value
test_jpgNoPipe = do
    res <- runErr$ processOneFile (fromJustNote "test_jpgNoPipe" corruptFile)
    assertBool (isRight res)   -- oporto has corrupt file
--    assertEqual (Right ["\nF: /home/frank/additionalSpace/Photos_2016/sizilien2016/DSC04129.JPG "]) res


test_proc = do
    procFile <- parseAbsDir "/proc"
    res <-   recurseDirUU procFile -- (fromJustNote "testdirabs" testDirAbs)
    assertEqual (Right []) res


