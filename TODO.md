# TODO

## Generic
* How to interface with the graphics?

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
* Goals
    * What programming model do we want for goals?
* Output
    * Maybe we should output the results as a JSON
      object straight from minizinc somehow?

## CP
* More types, like arrays and sets for an instance.
