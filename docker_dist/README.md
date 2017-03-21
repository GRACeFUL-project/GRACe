# Docker container

Instructions for building Docker-image with the GRACe server.

## Overview

| Filename              | Description                                        |
| --------------------- | -------------------------------------------------- |
| Dockerfile            | Dockerfile.                                        |
| README.md             | This document.                                     |
| build.sh              | Fetches dependencies and builds the Docker image.  |
| install-mzn-gecode.sh | Builds MiniZinc/Gecode dependencies inside the VM. |
| run.sh                | Runs the GRACe server inside the VM.               |

## Instructions

1. The server needs to be executable on some `x86_64` Ubuntu. A simple way to
   ensure this is to build the examples with Docker support enabled in the
   `stack.yaml`, i.e.

   ```
   docker:
     enable: true
   ```

2. Run `build.sh`. This will clone the libminizinc and gecode repositories and
   call `docker build`. You may be prompted to accept some expired certificate
   by subversion.
3. Follow [these](https://docs.docker.com/engine/getstarted/step_six/)
   instructions on how to push the image to dockerhub.
4. Run the image using `docker run -p 8081:8081 --rm eugraceful/grace-server`.
