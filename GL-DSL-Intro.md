# DSL for GRACeFUL Concept Maps

## Background

Following the Delft workshop we decided on revising the structure of the DSL
to be used in the modelling tools. The new DSL will be of increased expressivity
and should be able to capture the same type of constraint programs as expressed
in the CLOCWISe tool.

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
should be composable. The GL will be made up of DSL written components.

    More stuff here

## Current status

    Insert current status here.

At the time of writing (2016-11-28) there is a prototype including a GCM monad
for outlining "components" in the graph, such as specifying what connections
they have and of what type. Moreover, the components can be decorated with
constraints expressable in some MiniZinc compatible expression format.

## Examples

    Insert some example here.


## TBD

    Insert TBD here.
