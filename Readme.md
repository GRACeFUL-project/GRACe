# GRACeFUL generic library

## Installing

### Linux

[Install script](INSTALL.md)

### macOS

1. Download and install the complete MiniZinc distribution from
  [the MiniZinc page](http://www.minizinc.org/index.html).
2. The MiniZinc binaries will now be located under
  `/Applications/MiniZincIDE.app/Contents/Resources`. Add this directory to your
  path, like so:

      export PATH='$PATH:/Applications/MiniZincIDE.app/Contents/Resources'

## Running a constraint program

```shell
stack build
stack exec GenericLibrary
```

## Run test suite

Not to be confused with the GCM test framework.

```shell
stack test
```
