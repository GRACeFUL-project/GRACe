# TODO

## General
* Some form of graphical interface
* Should we hide much of the prelude
  to have nicer syntax?
    I think you can make a good argument
    for this, as the user is mean to write
    simple modules in the language and using
    the GUI to do the "gluing", thus there won't
    be that much mixing of normal Haskell and the DSL.

## GCM
* Generalise the return types for GCM components,
  so they can fit together easily
* Actions
    * What would be a good programming model to
      declare actions. Can we do it in the CP
      monad in some way? What about costs?
    * I have implemented ports with default values.
      Attached to each such port is a decision variable
      "var bool: a[ID]" and a constraint "(not a[ID]) ==> (v[ID] == defaultValue)".
      The plan is to set all of the parameter "a[ID]" variables
      to false at the end of compilation if the port
      has not been touched by an action. To figure out how to
      best implement this we need a programming model for actions.
* Fractional etc. instances

## CP
* Arrays and sets in the CP world
