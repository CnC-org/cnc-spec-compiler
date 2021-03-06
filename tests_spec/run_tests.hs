#!/usr/bin/env runhaskell 
{-# LANGUAGE ScopedTypeVariables #-}

-- A testing script that drives the execution of (spec-based) CnC
-- programs and compares their output to what's expected (using
-- various means of sorting and preprocessing the output).

-- This script assumes that all the programs are built before it is run.


import Control.Exception hiding (assert)
import Data.List
import Control.Monad
import HSH
import HSH.ShellEquivs
import Test.HUnit
import Debug.Trace
import qualified Data.Set as S

import System.Environment
import System.Exit 

-- import Text.Regex
import Text.Regex.Posix

----------------------------------------------------------------------------------------------------
--   First some global constants:
----------------------------------------------------------------------------------------------------

-- The root of the haskell_cnc working copy:
root = ".."  
-- The location of the cnc executable built by cabal:
-- cnc = root ++ "/dist/build/cnc/cnc"
-- Or in this case built by the Makefile (yuck, but no partial-target builds in cabal atm):
cnc = root ++ "/build/cnc"

-- Each test is associated with a line-preprocessor and a flag indicating whether it should succeed:
type Entry = (String, String -> String, Bool)
all_tests :: [Entry]
all_tests = 
 [
   ("tagfun_checking_valid", strip_refcounts, True)
 , ("tagfun_depends" ,       strip_refcounts, True)
 , ("simple_cycle" ,         strip_refcounts, True)
 , ("bigger_cycle",          strip_refcounts, True)
 , ("two_cycles",            strip_refcounts, True)
 , ("reduction_test",        strip_refcounts, True)
 , ("reduction_test2",       strip_refcounts, True)

 -- Known bad tests:
 , ("tagfun_checking_invalid", strip_refcounts, False)
 ]

-- TODO: This needs to be made a lot nicer...
-- Right now it just kills the tail ends of lines that deal with refcounts.
strip_refcounts =
--  traceFun "strip_refcounts" $
    killAfter "Decremented .* to" .
    killAfter  "Incrementing .* refcount to"
 

-- | Kill the rest of the line after a pattern match.
killAfter pat line = 
  case line =~ pat :: (String,String,String) of 
    (_,"",_) -> line
    (before,match,after) ->  before ++ match

--  let (before,match,after) = (line =~ )

fst3 (a,b,c) = a

----------------------------------------------------------------------------------------------------
--   next the main script that performs the test
----------------------------------------------------------------------------------------------------

main = do
  args <- getArgs
  putStrLn$ "Running tests of cnc translator..."
  tests <- case args of 
	       [] -> do putStrLn$ "Running *all* tests."
			return all_tests
	       [str] -> 
		 do let filtered = filter (isPrefixOf str . fst3) all_tests
		    putStrLn$ "Running a subset of tests: "++ show (map fst3 filtered)
		    return$ filtered
	       _ -> error "run_tests.hs script takes zero or one arguments"


  let myPut msg True  () = do putStrLn ""; putStrLn$ msg
      myPut msg False () = return ()
  runTestText (PutText myPut ()) $
    TestList [ test_cases "1" tests
	     , test_cases "4" tests
	     ]
  return ()

test_cases :: String -> [Entry] -> Test
test_cases numthreads tests = 
    testSet ("translator, num threads = "++numthreads) $ 
    map (each_test numthreads) tests

each_test :: String -> Entry -> Test
each_test numthreads (name, project, is_correct) = 
  testCase "" ("Running, " ++ numthreads ++" thread(s): "++ name ) $ test $
     do 
        -- Gather the stdout output from the test run:
        -- (TODO NOTE: Any good way to capture stderr here?)
        (output,syncaction) <- run$ setenv [("CNC_NUM_THREADS", numthreads)] $
 	                            ("./"++ name ++".exe") -|- tee [name++".out"]
        (s::String,exitcode) <- syncaction
	expected <- readFile$ name ++ ".cmpr"

        -- Convert the outputs to sets and do unordered (set) comparison:
        let s1 = S.fromList $ map project $ lines output
	    s2 = S.fromList $ map project $ lines expected 
	    set_eq = s1 == s2
	    fail_report = do
	       let d1 = S.difference s1 s2
		   d2 = S.difference s2 s1

	       putStrLn$ " FAIL: "
	       putStrLn$ "   Output string length "++ show (length (output::String)) 
			 ++", expected length " ++ show (length expected)

	       putStrLn$ "------------------------------------------------------------" 
	       putStrLn$ "     Diff1, in actual output but not expected: " 
	       putStrLn$ "------------------------------------------------------------" 
	       mapM_ putStrLn (S.toList d1)
	       putStrLn$ "------------------------------------------------------------" 
	       putStrLn$ "     Diff2, in expected but not actual output: " 
	       putStrLn$ "------------------------------------------------------------" 
	       mapM_ putStrLn (S.toList d2)
	       putStrLn$ "------------------------------------------------------------" 

        case (exitcode,is_correct) of
	  (ExitSuccess,True) -> do
	    when (not set_eq) fail_report
	    assert set_eq
	    putStrLn$ ""
	    return ()
	  (ExitFailure _,False) -> do
	    putStrLn "Failing test failed, as expected."
	    return ()
	  (_, True)  -> error "Test failed unexpectedly!"
	  (_, False) -> error "Erroneous test SUCCEEDED unexpectedly!" 
	  
----------------------------------------------------------------------------------------------------
-- To implement the above, here are some helpers for checking output.
----------------------------------------------------------------------------------------------------


-- TODO: exact comparison

-- TODO: line set comparison (sorted/unsorted)

lineSetEqual a b = 
  S.fromList (lines a) == S.fromList (lines b)

-- TODO: fuzzy number matching and/or regular expressions.


-- Tag a little bit more verbose output to the tests:
testCase prefix str tst = TestLabel lab (trace (tag++ lab) tst)
 where lab = if prefix == ""
             then str
	     else prefix ++ ": " ++ str
--       tag = " *** "
       tag = " [test] "

-- Likewise, identify the per-module sub-groups of tests
testSet name ls = 
    trace ("\n"++header ++"\n"++ spacer) (TestList ls)
 where header = "Running tests for " ++ show name 
       spacer = (take (length header) $ repeat '=')


traceFun :: (Show a, Show b) => String -> (a -> b) -> (a -> b)
traceFun name f = 
 trace (name ++": closure used.") $
 \ x -> 
   trace (name++": called with input "++ show x) $
     let result = f x in 
     trace (name++": result was:"++ show result) 
	   result


----------------------------------------------------------------------------------------------------
-- Currently UNUSED utilities
----------------------------------------------------------------------------------------------------

trans_all = do
  putStrLn$ "Translating several test .cnc files:"
  trans "tagfuns1.cnc"
  trans "tagfuns2.cnc"
  trans "tagfuns3.cnc"
  trans "cholesky.cnc"
  trans "mandel.cnc"
  trans "eigensolver.cnc"


dubquote str = "\"" ++ str ++ "\""
trans file = 
  do 
     putStrLn$ "\nTranslating .cnc file: "++ file
     putStrLn$ "================================================================================"
     let cmd = cnc ++ " trans " ++ dubquote file
     putStrLn$ "/Running command: " ++ cmd
     runIO$ cmd

