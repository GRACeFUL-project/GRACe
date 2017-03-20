# GRACe - a domain specific language for GRACeFUL Concept Maps

A DSL for GRACeFUL components.

## Installing

In order to use `GRACe`, the following software dependencies must be
met:

* [The MiniZinc distribution](http://www.minizinc.org/index.html)
* [GHC](https://www.haskell.org/downloads)

To ensure that library dependencies are exactly met, we recommend that
you use `stack`. `stack` handles the entire Haskell toolchain
(including the compiler GHC), library dependencies, building and
executing. Instructions for installing `stack` on macOS, Linux and
Windows can be found
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

### Installing MiniZinc: Linux

Patrik has provided an [install script](doc/INSTALL.md) for Linux.

### Installing MiniZinc: macOS

1. Download and install the complete MiniZinc distribution from
  [the MiniZinc page](http://www.minizinc.org/index.html).
2. The MiniZinc binaries will now be located under
  `/Applications/MiniZincIDE.app/Contents/Resources`. Add this directory to your
  path, like so:

      export PATH="$PATH:/Applications/MiniZincIDE.app/Contents/Resources"

## Running a constraint program

### Using sandboxes

Create a new sandbox and install the required dependencies.

```shell
cabal sandbox init
cabal update
cabal install --dependencies-only
```

Build and execute using

```shell
cabal build
cabal run examples
```

### Using stack

Stack will automatically take care of dependencies. Build and execute using

```shell
stack build
stack exec examples
```

Copy executables to `~/.local/bin` (put them on PATH) with

```shell
stack install
```

The executables will then be available from the command line. For example

```shell
example
```

### Testing using stack

Run entire test suite with

```shell
stack test
```

Load test modules in ghci by

```shell
stack ghci GRACe:test:test
```

Run test suite for a single module with

```shell
stack runghc test/TestModule.hs
```

This assumes there is a `main` function in `TestModule.hs`.

### Generate project documentation

Run haddock to generate project documentation

```shell
stack haddock --haddock-arguments --hyperlinked-source
```

To open the documentation directly append `--open`.

Find the path to the documentation by running

```shell
stack path --local-doc-root
```
