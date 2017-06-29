# GRACe - a domain specific language for GRACeFUL Concept Maps

A DSL for GRACeFUL components.

## Overview

| Directory    | Contents                                   |
| ------------ | ------------------------------------------ |
| doc/         | Various documentation.                     |
| docker_dist/ | Scripts for creating a Docker image.       |
| examples/    | Example programs written in GRACe.         |
| src/         | Haskell source code for the GRACe library. |
| test/        | Test suite                                 |

## Installation

Development of `GRACe` programs requires the following software dependencies 
to be met:

* [GHC][ghcurl]
* [The MiniZinc distribution][mzurl]

The recommended way of installing GHC and other Haskell libraries/tools are
through **stack**. **stack** handles the entire Haskell toolchain
(including the compiler GHC), library dependencies, building and
executing. Instructions for installing **stack** on macOS, Linux and Windows can 
be found [here][stackurl].

The recommended way of installing the required **MiniZinc** solver tools is 
through the bundled binary packages available [here][mzurl].

## Usage

First, clone the GRACe repository:

```shell
git clone https://github.com/GRACeFUL-project/GRACe grace
```

GRACe comes with a few examples that can be built using 

```shell
stack build
stack exec examples
```

**stack** will automatically take care of dependencies. Optionally, a small
test suite is available. Run the tests by executing

```shell
stack test
```

## Development

Currently there are no usage instructions for the GRACe language itself.
Instead, we refer to the source files [examples](examples/).

[stackurl]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[ghcurl]: https://www.haskell.org/downloads
[mzurl]: http://www.minizinc.org/software.html

