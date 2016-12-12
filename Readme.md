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

