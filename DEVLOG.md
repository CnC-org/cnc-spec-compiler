
DEVLOG
====================================================================================================

This is a log for day-to-day development issues.  It does not make
very good reading, but is helpful for search & reference by the
developers.


[2011.08.12] {Reducing Dependencies}
------------------------------------

Here are the packages that were installed on a clean system.  Well,
the ghc-6.12.3 isn't quick "clean"... the SOIC machines have more than
just the haskell-platform packages installed globally.  Hence there
are more packages for GHC 7.0.4:

    /usr/lib64/ghc-6.12.3/package.conf.d
      [... 76 globally installed packages ...]
    /u/rrnewton/.ghc/x86_64-linux-6.12.3/package.conf.d
	Graphalyze-0.11.0.0
	HTTP-4000.0.9
	HaXml-1.20.2
	ansi-terminal-0.5.5
	bktrees-0.3.1
	blaze-builder-0.3.0.1
	colour-2.3.1
	digest-0.0.0.9
	graphviz-2999.11.0.0
	haxr-3000.8.2
	hscurses-1.4.0.0
	hubigraph-0.3.2
	network-2.2.1.7
	pandoc-1.6.0.1
	parsec-3.1.1
	polyparse-1.4.1
	pretty-1.1.0.0
	prettyclass-1.0.0.0
	split-0.1.4.1
	stringtable-atom-0.0.6.1
	template-haskell-2.4.0.1
	texmath-0.4
	text-0.11.1.5
	transformers-0.2.2.0
	xml-1.3.9
	zip-archive-0.1.1.7


    /l/software/rhel6_x86_64/ghc-7.0.4/lib/ghc-7.0.4/package.conf.d
       Cabal-1.10.2.0
       array-0.3.0.2
       base-4.3.1.0
       bin-package-db-0.0.0.0
       bytestring-0.9.1.10
       containers-0.4.0.0
       directory-1.1.0.0
       extensible-exceptions-0.1.1.2
       ffi-1.0
       filepath-1.2.0.0
       ghc-7.0.4
       ghc-binary-0.5.0.2
       ghc-prim-0.2.0.0
       haskell2010-1.0.0.0
       haskell98-1.1.0.1
       hpc-0.5.0.6
       integer-gmp-0.2.0.3
       old-locale-1.0.0.2
       old-time-1.0.0.6
       pretty-1.0.1.2
       process-1.0.1.5
       random-1.0.0.3
       rts-1.0
       template-haskell-2.5.0.0
       time-1.2.0.3
       unix-2.4.2.0
    /u/rrnewton/.ghc/x86_64-linux-7.0.4/package.conf.d
       Graphalyze-0.11.0.0
       HTTP-4000.1.2
       HUnit-1.2.2.3
       HaXml-1.20.2
       ansi-terminal-0.5.5
       binary-0.5.0.2
       bktrees-0.3.1
       blaze-builder-0.3.0.1
       colour-2.3.1
       dataenc-0.14
       deepseq-1.1.0.2
       digest-0.0.0.9
       fgl-5.4.2.3
       graphviz-2999.11.0.0
       haxr-3000.8.2
       hscurses-1.4.0.0
       hubigraph-0.3.2
       mtl-2.0.1.0
       network-2.3.0.5
       pandoc-1.6.0.1
       parsec-3.1.1
       polyparse-1.4.1
       pretty-1.1.0.0
       prettyclass-1.0.0.0
       split-0.1.4.1
       stringtable-atom-0.0.6.1
       syb-0.3.3
       template-haskell-2.5.0.0
       texmath-0.4
       text-0.11.1.5
       transformers-0.2.2.0
       utf8-string-0.3.6
       xhtml-3000.2.0.1
       xml-1.3.9
       zip-archive-0.1.1.7
       zlib-0.5.3.1

36 packages for 7.0.4.  On top of the 26 built-in packages.  Actually
this system had LESS than the haskell-platform packages.  For 6.12.3
74 packages is a lot -- probably more than just Haskell-platform.

After stripping out the Graphalyze dependency, and reinstalling on a
fresh GHC-7.0.4 with -fBasicBuild only these 13 packages are installed
and it takes only 1 minute 27 seconds on basalt to do the whole thing.

      HUnit-1.2.2.3
      ansi-terminal-0.5.5
      binary-0.5.0.2
      fgl-5.4.2.3
      mtl-2.0.1.0
      parsec-3.1.1
      pretty-1.1.0.0
      prettyclass-1.0.0.0
      split-0.1.4.1
      stringtable-atom-0.0.6.1
      syb-0.3.3
      transformers-0.2.2.0
      zlib-0.5.3.1

It then took an additional five minutes to then reinstall without
-fBasicBuild.  Then CnC shows extra options for visualization.
That installed an extra 14 packages, some of which are in haskell platform.

      HTTP-4000.1.2
      HaXml-1.20.2
      blaze-builder-0.3.0.1
      colour-2.3.1
      dataenc-0.14
      deepseq-1.1.0.2
      graphviz-2999.11.0.0
      haxr-3000.8.2
      hubigraph-0.3.2
      network-2.3.0.5
      polyparse-1.4.1
      template-haskell-2.5.0.0
      text-0.11.1.5
      utf8-string-0.3.6


Specifically, after subtracting the haskell platform packages, the two
lists above should become. (Pretty is actually a core package... but
we demanded the newer version.)

      ansi-terminal-0.5.5
      binary-0.5.0.2
      pretty-1.1.0.0
      prettyclass-1.0.0.0
      split-0.1.4.1
      stringtable-atom-0.0.6.1

and:

      HaXml-1.20.2
      blaze-builder-0.3.0.1
      colour-2.3.1
      dataenc-0.14
      graphviz-2999.11.0.0
      haxr-3000.8.2
      hubigraph-0.3.2
      polyparse-1.4.1
      template-haskell-2.5.0.0
      utf8-string-0.3.6

Six and ten packages respectively.
    36 -> 27 -> 13 -> 6

