-----------------------------------------------------------------------------
--
-- Module      :   a test for HTF framework
-- insert {-@ HTF_TESTS @-} for each import
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main     where      -- must have Main (main) or Main where


--import System.Exit

import           Test.Framework
import {-@ HTF_TESTS @-} Lib.NoPipe
--import {-@ HTF_TESTS @-} Lib.MonadTransformer
--import {-@ HTF_TESTS @-} Lib.WithPipe
import {-@ HTF_TESTS @-} Lib.NoPipeUsingUniform
--import {-@ HTF_TESTS @-} Lib.PipeUsingUniform
import {-@ HTF_TESTS @-} Uniform.Error
import {-@ HTF_TESTS @-} Uniform.FileStrings



main =  do  -- with tests in other modules
    putStrLn "HTF ExampleTest.hs:\n"
    p <- htfMain htf_importedTests
    putStrLn ("HTF end StringConversion.hs test:\n" ++ show p)
    return ()

