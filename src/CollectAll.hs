-----------------------------------------------------------------------------
--
-- Module      :  a main for multiple test
-----------------------------------------------------------------------------

module Main     where      -- must have Main (main) or Main where

import Lib.NoPipe

main =  do  -- with tests in other modules
    putStrLn "Collect all files:\n"
    processDir "testFileIO"

    putStrLn ("----------------------------------------------------- end")
    return ()

