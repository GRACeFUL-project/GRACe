# GRACeFUL generic library

A DSL for GRACeFUL components.

## Installing

`GenericLibrary` requires MiniZinc. We provide installation instructions for
Linux and macOS.

### Linux

Patrik has provided an [install script](doc/INSTALL.md) for Linux.

### macOS

1. Download and install the complete MiniZinc distribution from
  [the MiniZinc page](http://www.minizinc.org/index.html).
2. The MiniZinc binaries will now be located under
  `/Applications/MiniZincIDE.app/Contents/Resources`. Add this directory to your
  path, like so:

      export PATH='$PATH:/Applications/MiniZincIDE.app/Contents/Resources'

## Running a constraint program

### Using sandboxes

Create a new sandbox and install the required dependencies.

```shell
cabal sandbox init
cabal install --dependencies-only
```

Build and execute using

```shell
cabal build
cabal run
```

### Using stack

Stack will automatically take care of dependencies. Build and execute using

```shell
stack build
stack exec GenericLibrary
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
stack ghci GenericLibrary:test:test
```

Run test suite for a single module with

```shell
stack runghc test/TestModule.hs
```

This assumes there is a main function in TestModule.hs.
