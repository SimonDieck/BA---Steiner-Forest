module GreedySteinerForest where

import Prelude
import Graph2


--Starts up the GreedyRoutine and evaluates its solution
greedyAlgo :: SteinerProblem -> (Maybe Int)
greedyAlgo (Problem p g) = let g'  = floydWarshall g
                               sol = greedyRoutine p g' [] in foldr mayplus (Just 0) (map (findEdg g) sol)

                               
 

--Approximates the Steiner forest problem by finding the pair with the shortest distance to each other and then updating all distances by setting the distance of the added edge to 0
greedyRoutine :: [(Int,Int)] -> DistGraph -> [Edge] -> [Edge]
greedyRoutine [] _ e = e
greedyRoutine x g es = let (Path e w p)  = findShortest g x
                           g'            = replacePaths (shortenViaEdg ([(a,b) | a <- [1..(gSizeP g)] , b <- [a..(gSizeP g)]]) e g) g
                           x'            = remove e x        
                       in greedyRoutine x' g' (p ++ es)
                       
                       
                       
--Finds the shortest Path in a Graph out of a list of Paths
findShortest :: DistGraph -> [(Int,Int)] -> Path
findShortest g [x]    = findP x g
findShortest g (x:xs) = shorterPath (findP x g) (findShortest g xs)



--If the Path via a chosen Edge whose length is set to 0 is shorter it will return this new shorter Path
shortenViaEdg :: [(Int,Int)] -> Edge -> DistGraph -> [Path]
shortenViaEdg [] _ _         = []
shortenViaEdg ((a,b):xs) (x,y) g = let (Path _ wa ax) = findP (a,x) g
                                       (Path _ wb yb) = findP (y,b) g
                                       (Path _ _ xy)  = findP (x,y) g
                                       newW = mayplus wa wb
                                   in if mayshorter newW (dist (findP (a,b) g)) then (Path (a,b) newW (ax ++ xy ++ yb)) : shortenViaEdg xs (x,y) g else shortenViaEdg xs (x,y) g
                                   
                                   
                                   
--TODO: Move to Util
remove :: Eq a => a -> [a] -> [a]
remove x (y:ys) | x == y   = ys
                |otherwise = y : (remove x ys)