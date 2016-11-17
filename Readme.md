# GRACeFUL generic library

## Installing
To install the required minizinc software grab
the minizinc distribution from here:
http://www.minizinc.org/software.html
Unpack the tarball and add the folder to your path.

## Running a constraint program
To generate a constraint program run:
```haskell
Main> writeFile "model.mzn" (compileGCM example)
```
This will generate the file "model.mzn".
To invoke the solver:
```shell
$> mzn-gecode model.mzn
```
