# DSL for GRACeFUL Concept Maps

## Background

Following the Delft work sprint we decided on revising the structure of the DSL
to be used in the modelling tools. The new DSL should be able to capture the
same type of constraint programs as expressed in the CLOCWISe tool. The key
contribution of the DSL will be a language for expressing reusable components
to support reasoning about systems like the small example from the Delft work sprint.

## Motivations

The following issues were brought up during the Delft work sprint:

* Solving the Casual Loop Diagrams (CLD) would not be as useful as initially 
  thought. It is still possible that they would still be valuable in the Group 
  Model Building sessions, however.
* The Generic Library (GL) is more important than previously thought.

The examples presented during Delft worksprint included several new types of
relations in the diagrams -- in short, these were no longer CLDs, but described
flows and storages of some quantity. Hence the need for a richer language able
to express these things (and others). Moreover, components written in the DSL
should be composable. The GL will be made up of components written in the DSL.

## Current status

Currently there is a prototype including a GCM monad for describing 
"components" in the graph, such as specifying what connections
they have and of what type. Moreover, the components can be decorated with
constraints expressed in the CP monad.

## Examples

```haskell
import CP
import Port
import GCM

-- A source of rain
rain :: Int -> GCM (Port Int)
rain s = 
    do
      p <- createPort
      set p s
      return p 

-- A pump with a fixed capacity
pump :: Int -> GCM (Port Int)
pump c =
    do
        flow <- createPort
        component $ do
                      f <- value flow
                      assert  $ f `inRange` (0, lit c)
        return flow

-- A storage with a fixed capacity and a pump
storage :: Int -> GCM (Port Int, Port Int, Port Int)
storage c =
    do
        inflow   <- createPort
        pump     <- createPort
        overflow <- createPort
        component $ do
                      inf <- value inflow
                      pmp <- value pump
                      ovf <- value overflow
                      assert $ ovf === max' 0 (inf - pmp - lit c)
                      let sumFlow = ovf + pmp
                      assert $ inf `inRange` (sumFlow, sumFlow + lit c)
        return (inflow, pump, overflow)

-- Small example
example :: GCM ()
example =
    do
      -- Instantiate components
      r <- rain 10
      p <- pump 7
      (inf, pmp, ovf) <- storage 4

      -- Link ports
      link inf r
      link p pmp

      -- Minimise overflow
      g <- createGoal
      fun (\overflow -> negate overflow) ovf g

      -- Output the solution
      output p "pump operation"
```
We run the small example
```
ghci> runGCM example
pump operation = 6
--------
========
``` 

## Questions
* What are the user interface requirements? (We have a pretty good idéa, but we want to make sure we are on the same page)
* Can we get some example constraints from CLOCKWISe from T
* Can we get someone to try it out and give some feedback?
