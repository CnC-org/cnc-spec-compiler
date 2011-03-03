#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- MUST be run from the directory containing this script.

import System.Environment
import NightlyTester 
import HSH

repo = "Haskell-CnC"

----------------------------------------------------------------------------------------------------
-- Main Script

main =  
   getArgs >>= \ emails ->
   runNightlyTest (mkDefaultConfig repo emails) $ 
 do 

  gitReportHead
  reportMachineInfo

  section "Clean and build everithing"
--  mrun "make clean"
--  mrun "cabal configure"
--  mrun "cabal build"

-- Not going for the whole shebang yet:
--  section "Haskell CnC Runtime Tests - Unit and System"
--  mrun "./dist/build/haskell-cnc-runTests/haskell-cnc-runTests"

  -- section "Haskell CnC Runtime Example programs"

  section "Spec-Compiler Unit Tests:"
  mrun "./dist/build/cnc/cnc --selftest"

  -- section "Spec-Compiler System Tests:"
  -- cd   "tests_spec"
  -- mrun "make test"

  newline


