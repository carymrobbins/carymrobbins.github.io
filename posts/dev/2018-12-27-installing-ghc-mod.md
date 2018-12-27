---
title: Installing ghc-mod for newer GHC versions
---

For GHC versions >= 8.2, getting a working ghc-mod is difficult.
Lucky for us, Alan Zimmerman has done the hard work of getting ghc-mod working with GHC
8.2, 8.4, and 8.6 as part of the [Haskell IDE Engine](https://github.com/haskell/haskell-ide-engine) project.

There's also news of a successor to ghc-mod, [Happy Haskell Programming](https://github.com/kazu-yamamoto/hhp)
being in the works by Kazu himself.

For those of us using tooling which relies on ghc-mod, here are some steps to getting a working
ghc-mod for GHC >= 8.2 thanks to Alan's fork.

First, either clone Alan's fork or add the fork as a remote to your existing ghc-mod clone -

```
% git clone https://github.com/alanz/ghc-mod
% cd ghc-mod
```

Next, checkout the appropriate branch for the desired GHC version. Note that ghc-mod only
works for a given project when both ghc-mod and the project are compiled with the same version
of GHC.

```
% git checkout ghc-8.4-hie
```

You'll need the `cabal` binary (a.k.a. `cabal-install`). Get it via whichever means you need.
I generally use stack, so I'll just use that for now.

```
% stack install cabal-install

% cabal --version
cabal-install version 2.2.0.0
compiled using version 2.2.0.1 of the Cabal library
```

Now we need to initialize our cabal build. I recommend using a sandbox
so your ghc-mod build doesn't affect global package state.

```
% cabal update
% cabal sandbox init
```

Next, we need to tell cabal about the ghc-mod-core dependency, which
is located in the same repo under the `core` subdirectory.

```
% cabal sandbox add-source core
```

We'll need the `happy` binary used by `haskell-src-exts` during
its parser generation.

```
% cabal install happy
```

Now we can install the `ghc-mod` binary into our sandbox.

```
% cabal install
```

Optionally, you can copy the `ghc-mod` binary to somewhere easily
accessible and namespaced for the GHC version.

```
cp .cabal-sandbox/bin/ghc-mod ~/bin/ghc-mod-8.4
```

And you're done! I recommend running a test command from the
terminal to confirm that it works properly with your project.

```
% cd path/to/your/project
% ghc-mod-8.4 check File.hs
```

At this point I did run into an issue with integer-gmp on OSX -

```
lookupSymbol failed in relocateSection (RELOC_GOT)
/Users/me/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0/HSinteger-gmp-1.0.2.0.o: unknown symbol `___gmp_rands'
unable to load package `integer-gmp-1.0.2.0'
```

To get around this I simply hid the object file based on recommendations
from this GHC ticket -

https://ghc.haskell.org/trac/ghc/ticket/15105

```
% cd /Users/me/.stack/programs/x86_64-osx/ghc-8.4.3/lib/ghc-8.4.3/integer-gmp-1.0.2.0

% mv {HSinteger-gmp-1.0.2.0.o{,_DISABLE_GHC_ISSUE_15105}
```
