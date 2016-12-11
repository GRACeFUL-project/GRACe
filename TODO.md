# TODO

## General
* Hide (qualify) parts of the Prelude since we're not going to use it as much;
  provides nicer syntax for operating directly on `CPExp` things.

## GCM
* Generalise the return types for GCM components, so they can fit together 
  easily.

## CP
* Arrays and sets in the CP world.

## Compilation
* Compilation stages. We're unsure of what exactly MiniZinc does at this point,
  but what we have seen suggests that we should at least implement some simple
  optimizations such as constant folding, et cetera. Also, we would like to
  minimize the number of variables, and the size of the generated code.
* Add comments to generated code for traceability.

## Testing
* Tests for code generation.
* Regression testing further ahead.
* Property based testing of user code (generators).

### By priority
0. A "main" function
0. Examples
1. Shrinking
2. Informative output
3. QuickCheck-style statistics gathering
