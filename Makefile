

# WARNING: This Makefile is largely redundant with the .cabal file and will be deleted at some point.

# It has some crufty shortcuts used by the developers.

#====================================================================================================

all: release

ifeq (,$(GHC))
  GHC= ghc
endif

#====================================================================================================
# Below is the (entirely separate) makefile for the translator.
#====================================================================================================

HSOURCE=SrcLoc.hs Main.hs GatherGraph.hs AST.hs Codegen/CppOld.hs Codegen/Haskell.hs \
        CncGraph.hs CncViz.hs TraceVacuum.hs Curses.hs Util.hs \
        Passes/ReadHarch.hs Passes/ReadCnC.hs Passes/TypeDefs.hs

ifeq (,$(BUILDDIR))
  BUILDDIR= ./build/
endif
#BUILDDIR= ./
HCNCNAME=cnc

$(BUILDDIR):
	mkdir $(BUILDDIR)

# This is for use in the context of a complete Intel CnC distribution.
install: 
	rm -f Intel/Cnc/Spec/Version.hs
#	$(MAKE) Intel/Cnc/Spec/Version.hs $(HCNCNAME).stripped
#	cp $(BUILDDIR)/$(HCNCNAME).stripped `which $(HCNCNAME)`
	$(MAKE) Intel/Cnc/Spec/Version.hs release
	if [ -e ../../distro/ ]; then mkdir -p ../../distro/bin/$(CNC_ARCH_PLATFORM)/; fi
	if [ -e ../../distro/ ]; then cp $(BUILDDIR)/$(HCNCNAME).release ../../distro/bin/$(CNC_ARCH_PLATFORM)/cnc; fi

trans_inplace:
	GHCFLAGS="$(GHCFLAGS) -c" BUILDDIR=./ $(MAKE) ./$(HCNCNAME)

trans: 
	@echo 
	@echo ================================================================================
	@echo   Building Spec Translator/Compiler.
	@echo ================================================================================
	$(MAKE) $(BUILDDIR)/$(HCNCNAME)

Intel/Cnc/Spec/Version.hs: 
	runhaskell ./scripts/extract_version.hs

release:
	GHCFLAGS="-rtsopts -O2" $(MAKE) $(BUILDDIR)/$(HCNCNAME).stripped
	@echo Packing executable with UPX:
	rm -f $(BUILDDIR)/$(HCNCNAME).release
	upx $(BUILDDIR)/$(HCNCNAME).stripped -o $(BUILDDIR)/$(HCNCNAME).release

$(BUILDDIR)/$(HCNCNAME).stripped: viz
	@echo Stripping executable to reduce size.
	strip $(BUILDDIR)/$(HCNCNAME) -o $(BUILDDIR)/$(HCNCNAME).stripped

$(BUILDDIR)/$(HCNCNAME): $(BUILDDIR) preproc buildtrans
buildtrans: 
	$(GHC) $(GHCFLAGS) --make Intel/Cnc/Spec/Main.hs -odir $(BUILDDIR) -o $(BUILDDIR)/$(HCNCNAME) -fwarn-unused-imports

viz: $(BUILDDIR) $(BUILDDIR) preproc
#	$(GHC) $(GHCFLAGS) -c Intel/Cnc/Spec/CncLexer.hs 
	$(GHC)             -odir $(BUILDDIR) -c Intel/Cnc/Spec/CncLexer.hs 
	$(GHC) $(GHCFLAGS) -DCNCVIZ --make Intel/Cnc/Spec/Main.hs -odir $(BUILDDIR) -o $(BUILDDIR)/$(HCNCNAME)

preproc: Intel/Cnc/Spec/CncLexer.hs Intel/Cnc/Spec/CncGrammar.hs

Intel/Cnc/Spec/CncLexer.hs: Intel/Cnc/Spec/CncLexer.x
	alex Intel/Cnc/Spec/CncLexer.x

Intel/Cnc/Spec/CncGrammar.y: Intel/Cnc/Spec/CncGrammar.y.pp
	cpp -P -CC $^ $@ 
#	gcc -x c -E $^ -o $@ 

Intel/Cnc/Spec/CncGrammar.hs: Intel/Cnc/Spec/CncGrammar.y
	happy  Intel/Cnc/Spec/CncGrammar.y


wctrans:
	(cd Intel/Cnc/Spec/; ln -f -s CncLexer.x CncLexer.temp.hs)
	(cd Intel/Cnc/Spec/; ln -f -s CncGrammar.y.pp CncGrammar.temp.hs)
	(cd Intel/Cnc/Spec/; cloc-1.08.pl --by-file CncLexer.temp.hs CncGrammar.temp.hs $(HSOURCE))

#====================================================================================================

clean: cleanruntime cleantrans cleantests
	runhaskell ./Setup.hs clean

cleanruntime:
	rm -f ./Intel/*.o ./Intel/*.hi Intel/*~ 
	rm -f ./Intel/Cnc/*.o Intel/Cnc/*.hi 
	rm -f $(BUILDDIR)/Intel/*.o      $(BUILDDIR)/Intel/*.hi 
	rm -f $(BUILDDIR)/Intel/Cnc/*.o  $(BUILDDIR)/Intel/Cnc/*.hi 
	rm -f *.aux little*.log 
	(cd examples; $(MAKE) clean)

cleantrans:
	rm -rf $(BUILDDIR)/$(HCNCNAME)  $(BUILDDIR)/$(HCNCNAME).* 
	(cd $(BUILDDIR)/Intel/Cnc/Spec;  find -name "*.o" | xargs -i rm {} )
	(cd $(BUILDDIR)/Intel/Cnc/Spec;  find -name "*.hi" | xargs -i rm {} )
	(cd ./Intel/Cnc/Spec;  find -name "*.o" | xargs -i rm {} )
	(cd ./Intel/Cnc/Spec;  find -name "*.hi" | xargs -i rm {} )

cleantests:
	(cd tests_spec; $(MAKE) clean)


#====================================================================================================

pkgtest:
	rm -rf dist/cnc-spec-compiler-*
	cabal sdist
	(cd dist; tar xzvf cnc-spec-compiler-*.tar.gz)
	(cd dist/cnc-spec-compiler-*/ && cabal install)

# cd graphPartitioner; $(MAKE)

# Prevent odd make builtin rules re: cnc.sh
.SUFFIXES:
