# GRACe - a domain specific language for GRACeFUL Concept Maps

A DSL for GRACeFUL components.

## Overview

| Directory    | Contents                                   |
| ------------ | ------------------------------------------ |
| doc/         | Various documentation.                     |
| docker_dist/ | Scripts for creating a Docker image.       |
| examples/    | Example programs written in GRACe.         |
| src/         | Haskell source code for the GRACe library. |
| test/        | Test suite.                                |

## Installation for users
We provide a platform-independent Docker image containing an executable
for the ``OilCrops`` example, written in GRACe.

Running the example requires the Docker Community Edition (CE)
to be installed. Docker CE, as well as installation instructions are available
at [the docker website][dockerurl]. Once Docker CE is installed,
the example can be executed using the Docker application as follows: 

Open a terminal (the command prompt for Windows users) and execute the commands

```
  docker pull eugraceful/grace-examples:latest
  docker run --rm eugraceful/grace-examples:latest
```
This will run the ``OilCrops`` example and write the problem solution to
standard output.

This example contains a small optimization problem in
which the objective is to dedicate a set amount of farmland area to
three different crops, with the goal of maximizing the yield of
vegetable oil produced from these crops. A description of the
example can be found in the [tutorial] and the source code is [here][OilCropsCode].

## Installation for developers

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
stack exec SomeExamples
```

**stack** will automatically take care of dependencies. Optionally, a small
test suite is available. Run the tests by executing

```shell
stack test
```

## Development

The [examples](examples/) folder contains several examples of GRACe programs. 
In addition to these, we provide a short tutorial on how to write GRACe 
programs [here][tutorial].

[tutorial]: https://github.com/GRACeFUL-project/DSL-WP/blob/master/tutorial/pdf/GRACeTutorial.pdf
[stackurl]: https://docs.haskellstack.org/en/stable/install_and_upgrade/
[ghcurl]: https://www.haskell.org/downloads
[mzurl]: http://www.minizinc.org/software.html
[OilCropsCode]: https://github.com/GRACeFUL-project/GRACe/blob/master/examples/OilCrops.hs
[dockerurl]: https://www.docker.com/products/docker

