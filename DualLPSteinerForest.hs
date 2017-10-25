module DualLPSteinerForest where

import Prelude
import Graph2


--Need a structure to find/remove and add edges to a structure fast as well as unite to structures into one as well as seperating them
data CStruct = Undef Node


type Node = Int

--Find all Outgoing Edges of a strucutre
findDelta :: CStruct -> [Edge]
findDelta = undefined


--Returns True if two Nodes are both part of a Component
containsST :: CStruct ->  (Node,Node) -> Bool
containST c (a,b) = (containsNode c a) && (containsNode c b)


--Checks if a component contains a certain node
containsNode :: CStruct -> Node -> Bool
containsNode = undefined


--Combines to components into one
union :: CStruct -> CStruct -> CStruct
union = undefined


--Removes an Edge from the component
removeEdg :: CStruct -> Edge -> CStruct
removeEdg = undefined


--Splits a Structure if possible of at a Node
splitAt :: CStruct -> Node -> (Either CStruct (CStruct,CStruct))
splitAt = undefined