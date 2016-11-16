# TODO

## GCM
* Actions
* Goals
* Code-Generation
* Generalise the pattern of
  creating a GCM compoenent from
  a function like `Port x -> Port y -> Port z -> CP ()`
  by using a generalised product type
  and some typeclass magic

## CP
* Code-Generation

## General notes
Maybe we need to be able to expose parameters, for two reasons:
1. To be able to affect them using actions
2. To be able to pose constraints like:
   Make the pump go at full capacity when
   possible
