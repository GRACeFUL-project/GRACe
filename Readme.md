# GRACeFUL generic library

## Installing
To install the required minizinc software grab
the minizinc distribution from here:
https://github.com/MiniZinc/MiniZincIDE/releases/download/2.1.0/MiniZincIDE-2.1.0-bundle-linux-x86_64.tgz
Unpack the tarball and add the folder to your path.

## Running a constraint program
To generate a constraint program run:
```
Main> writeFile "model.mzn" (compileGCM example)
```
This will generate the file "model.mzn".
To invoke the solver:
```
$> mzn-gecode model.mzn
```
