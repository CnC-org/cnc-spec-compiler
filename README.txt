
---------------------------------------------
Concurrent Collections Specification Compiler
---------------------------------------------
 Author: Ryan Newton, Copyright 2009-2011


This directory contains an implementation of a compiler/translator for
program specifications in the Concurrent Collections programming model
(CnC).  These specifications are not full programs.  Rather, they
delineate the communication between components ("steps") that are
written in another language such as C++ or Haskell.

You will need to have a compatible CnC runtime implementation (such as
Intel Concurrent Collections for C++) that is distributed separately
from this program.

If you are looking in this directory, you are probably not using this
package through cabal.  Currently, this directory contains a Makefile
and other scripts that are redundant with the cabal file and will be
removed in the future.

----------------------------------------------
 Building and running the CnC Spec Compiler:
----------------------------------------------

The main entrypoint for the Spec tool is Intel/Cnc/Spec/Main.hs.

OPTION 1: Type "make"

OPTION 2: Try ghc --make directly

OPTION 3: Try "cabal install" in this directory.


----------------------------------------------
 Unit and system tests:
----------------------------------------------

Once you have built the "cnc" executable, you can run unit tests with: 

    cnc --selftest

The system tests are under the ./tests_spec/ directory.  That
directory contains its own Makefile which supports building and
running:

    cd tests_spec
    make 
    make run

You may also refer to the README in that directory.
