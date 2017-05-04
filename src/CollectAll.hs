-----------------------------------------------------------------------------
--
-- Module      :  a main for multiple test
-----------------------------------------------------------------------------

module Main     where      -- must have Main (main) or Main where

import Lib.NoPipe
import Lib.WithPipe
import Lib.NoPipeUsingUniform
main =  do  -- with tests in other modules
    putStrLn "Collect all files:\n"
--    processDir "/home/frank"
--    startPipe "/home/frank/Workspace8/testDirFileIO"
--    startPipe "/home/frank/Workspace8"
--    startPipe "/home/frank/Workspace8/pipeExample/testDirFileIO"
--    startPipe "/"
    recurseDirUU "/"   -- no pipe, uniform
    putStrLn ("----------------------------------------------------- end")
    return ()

