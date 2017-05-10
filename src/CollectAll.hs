-----------------------------------------------------------------------------
--
-- Module      :  a main for multiple test
-----------------------------------------------------------------------------

module Main     where      -- must have Main (main) or Main where

--import Lib.NoPipe
--import Lib.WithPipe
import Lib.NoPipeUsingUniform
--import Lib.PipeUsingUniform
import Path


main =  do  -- with tests in other modules
    putStrLn "\n\n\n\nCollect all files: bb \n"
--    processDir "/home/frank"
--    startPipe "/home/frank/Workspace8/testDirFileIO"
--    startPipe "/home/frank/Workspace8"
--    startPipe "/"  -- fails for /proc


--    startPipe "/" "alldiskList2"
--    startPipe "/home" "alldiskList2"
    targetDir = parseAbsDir "/home'
    resultFile = parseRelFile "alldiskList2"
    recurseDirUU targetDir

--    recurseDirUU "/"   -- no pipe, uniform
    putStrLn ("----------------------------------------------------- end")
    return ()

