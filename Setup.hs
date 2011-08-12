#!/usr/bin/env runhaskell


-- module Main where
-- import Distribution.Simple
-- main :: IO ()
-- main = defaultMain


import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit

--main = defaultMainWithHooks simpleUserHooks
-- --defaultUserHooks

main :: IO () 
main = do putStrLn$ "Running Setup.hs ..."
	  defaultMainWithHooks 
	   simpleUserHooks 
	   -- (simpleUserHooks  {
	   --        --, hookedPreProcessors= (mypp : hookedPreProcessors hooks) 
	   -- 	  }) 

-- mypp :: PPSuffixHandler
-- mypp = (".y.pp", \ _ _ -> ppTestHandler)

-- ppTestHandler :: PreProcessor
-- ppTestHandler =
--    PreProcessor {
--      platformIndependent = True,
--      runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
--        do putStrLn$ (inFile++" has been preprocessed to "++outFile)
--           stuff <- readFile inFile
--           writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
--           return ()
--    }


