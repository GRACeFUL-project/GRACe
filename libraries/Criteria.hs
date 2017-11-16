module Criteria (library) where

import Library
import qualified CLDlib  -- assuming CLDlib exports funNode

-- this file contains some standard criteria for CRUD
-- the implementation of the elements is provisional (just a copy of the CLDlib criterion)
library :: Library
library = Library "criteria"
  [
    Item "costcriterion" ["description: Cost", "imgURL: ./data/img/costcriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 8", "layer: causal"] $
      costcriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "flooddamagecriterion" ["description: Flood Damage", "imgURL: ./data/img/flooddamagecriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 1", "layer: causal"] $
      flooddamagecriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "floodnuisancecriterion" ["description: Flood Nuisance", "imgURL: ./data/img/floodnuisancecriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 2", "layer: causal"] $
      floodnuisancecriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "greenbluecriterion" ["description: Amount of Green/Blue Space", "imgURL: ./data/img/greenbluespacecriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 7", "layer: causal"] $
      greenbluecriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "centralparkingcriterion" ["description: Amount of Parking Space on Main Square", "imgURL: ./data/img/centralparkingcriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 3", "layer: causal"] $
      parkingcriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "parkingcriterion" ["description: Total Amount of Parking Space", "imgURL: ./data/img/parkingcriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 4", "layer: causal"] $
      parkingcriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "roadaccesscriterion" ["description: Road Access during Flood Events", "imgURL: ./data/img/roadaccesscriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 6", "layer: causal"] $
      roadaccesscriterion ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
  , Item "trafficcriterion" ["description: Traffic Capacity (normal)", "imgURL: ./data/img/trafficcriterion.png", "graphElement: nodal", "superClass: criterion", "nr: 5", "layer: causal"] $
      CLDlib.funNode ::: "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple3 ("rotation: false" # "incomingType: none" # "outgoingType: none" # "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
           )
    ]

costcriterion = CLDlib.funNode
flooddamagecriterion = CLDlib.funNode
floodnuisancecriterion = CLDlib.funNode
greenbluecriterion = CLDlib.funNode
centralparkingcriterion = CLDlib.funNode
parkingcriterion = CLDlib.funNode
roadaccesscriterion = CLDlib.funNode
trafficcriterion = CLDlib.funNode

