# Development Console log

```console
$ cabal init

Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
Package name? [default: toy-robot-haskell]
Package version? [default: 0.1.0.0]
Please choose a license:
   1) GPL-2
   2) GPL-3
   3) LGPL-2.1
   4) LGPL-3
   5) AGPL-3
   6) BSD2
 * 7) BSD3
   8) MIT
   9) ISC
  10) MPL-2.0
  11) Apache-2.0
  12) PublicDomain
  13) AllRightsReserved
  14) Other (specify)
Your choice? [default: BSD3]
Author name? [default: Alex Sonneveld]
Maintainer email? [default: Alex.Sonneveld@live.com.au]
Project homepage URL? https://github.com/Sonna/toy-robot-haskell
Project synopsis? The infamous Toy Robot code test in Haskell
Project category:
 * 1) (none)
   2) Codec
   3) Concurrency
   4) Control
   5) Data
   6) Database
   7) Development
   8) Distribution
   9) Game
  10) Graphics
  11) Language
  12) Math
  13) Network
  14) Sound
  15) System
  16) Testing
  17) Text
  18) Web
  19) Other (specify)
Your choice? [default: (none)]
What does the package build:
   1) Library
   2) Executable
Your choice? 2
What is the main module of the executable:
 * 1) Main.hs (does not yet exist, but will be created)
   2) Main.lhs (does not yet exist, but will be created)
   3) Other (specify)
Your choice? [default: Main.hs (does not yet exist, but will be created)]
Source directory:
 * 1) (none)
   2) src
   3) Other (specify)
Your choice? [default: (none)] 2
What base language is the package written in:
 * 1) Haskell2010
   2) Haskell98
   3) Other (specify)
Your choice? [default: Haskell2010]
Add informative comments to each field in the cabal file (y/n)? [default: n] y

Guessing dependencies...

Generating LICENSE...
Generating Setup.hs...
Generating ChangeLog.md...
Generating src/Main.hs...
Generating toy-robot-haskell.cabal...

You may want to edit the .cabal file and add a Description field.
```

```console
$ cabal sandbox init
Writing a default package environment file to
/Users/Sonna/Projects/haskell/toy-robot-haskell/cabal.sandbox.config
Creating a new sandbox at
/Users/Sonna/Projects/haskell/toy-robot-haskell/.cabal-sandbox

$ cabal install -j
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
Resolving dependencies...
Notice: installing into a sandbox located at
/Users/Sonna/Projects/haskell/toy-robot-haskell/.cabal-sandbox
Configuring toy-robot-haskell-0.1.0.0...
Building toy-robot-haskell-0.1.0.0...
Installed toy-robot-haskell-0.1.0.0
```

```console
$ cabal update
Downloading the latest package list from hackage.haskell.org

```

```console
$ cabal install --enable-tests
Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal
update' to download it.
Resolving dependencies...
Notice: installing into a sandbox located at
/Users/Sonna/Projects/haskell/toy-robot-haskell/.cabal-sandbox
Configuring toy-robot-haskell-0.1.0.0...
Building toy-robot-haskell-0.1.0.0...
Installed toy-robot-haskell-0.1.0.0
```

# References:
- [How to write a Haskell program \- HaskellWiki]
  (https://wiki.haskell.org/How_to_write_a_Haskell_program)
