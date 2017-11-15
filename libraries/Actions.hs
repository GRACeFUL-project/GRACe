module Actions (library) where

import Library
import qualified CLDlib  -- assuming CLDlib exports actionNode

-- this file contains some typical actions for CRUD
-- the implementation of the elements is provisional (just a copy of the CLDlib action)

library :: Library
library = Library "actions"
  [
    Item "bioswaleStreetAction" ["description: Apply Bioswale to Street", "imgURL: ./data/img/bioswaleStreetAction.png", "graphElement: nodal", "superClass: action", "crit: 2,2,0,0,-1,0,2,-2", "layer: causal"] $
      bioswaleStreetAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                     "cost" # tPort tInt)
           )
   ,Item "bioswaleParkingAction" ["description: Apply Bioswale to Paved Parking", "imgURL: ./data/img/bioswaleParkingAction.png", "graphElement: nodal", "superClass: action", "crit: 2,2,-1,-1,0,0,2,-2", "layer: causal"] $
      bioswaleParkingAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                           "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                           "cost" # tPort tInt)
           )
   ,Item "bioswaleGreenSpaceAction" ["description: Apply Bioswale to Green Space", "imgURL: ./data/img/bioswaleGreenSpaceAction.png", "graphElement: nodal", "superClass: action", "crit: 2,2,0,0,0,0,0,-1", "layer: causal"] $
      bioswaleGreenSpaceAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                           "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                           "cost" # tPort tInt)
           )
   ,Item "makeParkingFloodableAction" ["description: Make Parking Floodable", "imgURL: ./data/img/makeParkingFloodableAction.png", "graphElement: nodal", "superClass: action", "crit: 2,1,0,0,0,0,0,-2", "layer: causal"] $
      makeParkingFloodableAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                     "cost" # tPort tInt)
           )
   ,Item "floodableParkingOnGreenSpaceAction" ["description: Build Floodable Parking on Green Space", "imgURL: ./data/img/floodableParkingOnGreenSpaceAction.png", "graphElement: nodal", "superClass: action", "crit: 2,2,0,2,0,0,-1,-1", "layer: causal"] $
      floodableParkingOnGreenSpaceAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                           "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                           "cost" # tPort tInt)
           )
   ,Item "publicGreenRoofAction" ["description: Apply Green Roof to Public Building", "imgURL: ./data/img/publicGreenRoofAction.png", "graphElement: nodal", "superClass: action", "crit: 1,0,0,0,0,0,1,-1", "layer: causal"] $
      publicGreenRoofAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                           "value" # tPort tSign)
                          ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                           "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                           "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                          ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                           "cost" # tPort tInt)
           )
   ,Item "privateGreenRoofAction" ["description: Apply Green Roof to Private Building", "imgURL: ./data/img/privateGreenRoofAction.png", "graphElement: nodal", "superClass: action", "crit: 1,0,0,0,0,0,1,-1", "layer: causal"] $
      privateGreenRoofAction ::: "values" # tList tSign .-> "costs" # tList tInt .->
                     "numIn" # tInt .-> "numOut" # tInt .->
      tGCM (tTuple4 ("rotation: false" # "incomingType: none" # "outgoingType: none" #
                     "value" # tPort tSign)
                    ("rotation: true" # "incomingType: arbitrary" # "outgoingType: none" #
                     "incoming" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: true" # "incomingType: none" # "outgoingType: arbitrary" #
                     "outgoing" # tList (tPair (tPort tSign) (tPort tSign)))
                    ("rotation: false" # "incomingType: none" # "outgoingType: arbitrary" #
                     "cost" # tPort tInt)
           )
    ]



bioswaleStreetAction               = CLDlib.actionNode
bioswaleParkingAction              = CLDlib.actionNode
bioswaleGreenSpaceAction           = CLDlib.actionNode
makeParkingFloodableAction         = CLDlib.actionNode
floodableParkingOnGreenSpaceAction = CLDlib.actionNode
publicGreenRoofAction              = CLDlib.actionNode
privateGreenRoofAction             = CLDlib.actionNode

