----------------------------------------------------------------------------
--
-- Module      :  a main for multiple test
-----------------------------------------------------------------------------

module Main     where      -- must have Main (main) or Main where

--import Lib.NoPipe
--import Lib.WithPipe
import Lib.NoPipeUsingUniform
import Lib.PipeUsingUniform
import Path
import Safe


main =  do  -- with tests in other modules
    putStrLn "\n\n\n\nCollect all files: bb \n"
--    processDir "/home/frank"
--    startPipe "/home/frank/Workspace8/testDirFileIO"
--    startPipe "/home/frank/Workspace8"
--    startPipe "/"  -- fails for /proc


    startPipe (mkFilenameAbsDir "/") (mkFilenameAbsFile "/home/frank/alldiskList2")
--    startPipe "/home" "alldiskList2"
--
--    let     targetDir = fromJustNote "collectall_targetDir" $ parseAbsDir "/home"
----            resultFile = parseRelFile "alldiskList2"
--    recurseDirUU targetDir

--    recurseDirUU "/"   -- no pipe,   uniform
    putStrLn ("----------------------------------------------------- end")
    return ()
