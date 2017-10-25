module Graph2 where

import Prelude
import Data.Array



--Defines a Graph by it's Size as well as the weights of the Edges, with the Weights returning 'Nothing' if there is no edge
data Graph = Graph Int Weights


--Defines weights as a function that maps a pair of Integers to either a Value or 'Nothing'. This allows for Adjacencylists and Matrices to both be used as a representation of the Graph. REMINDER: When adding different representations update gUpdate
data Weights = SimpleW (Edge -> (Maybe Int)) | ArrayW (Array (Int,Int) (Maybe Int))

    
type Edge = (Int,Int)


data SteinerProblem = Problem [(Int,Int)] Graph


data Path = Path (Int,Int) (Maybe Int) [Edge]


data DistGraph = Dist Int (Array (Int,Int) Path)



--Returns the number of Edges in the Graph
gSize :: Graph -> Int
gSize (Graph i _) = i



gSizeP :: DistGraph -> Int
gSizeP (Dist i _) = i


--Returns the length of a Path
dist :: Path -> (Maybe Int)
dist (Path _ d _) = d



convertToDist :: Graph -> DistGraph
convertToDist (Graph i w) = let p = [ ((a,b),(Path (a,b) (findEdg (Graph i w) (a,b)) [(a,b)] )) | a <- [1..i] , b <- [1..i]] in Dist i (array ((1,1),(i,i)) p)



--Returns the weight of and Edge if it exist, 'Nothing' otherwise
findEdg :: Graph -> Edge -> (Maybe Int)
findEdg (Graph i (SimpleW w)) (a,b) | (a>i) || (b>i) = Nothing
                                    | otherwise      = w (a,b)
findEdg (Graph i (ArrayW aw)) (a,b) | (a>i) || (b>i) = Nothing
                                    | otherwise      = aw ! (a,b)
                                    
                                    
                                    
findP :: (Int,Int) -> DistGraph -> Path
findP (a,b) (Dist i p) = p ! (a,b)
                          

--Replaces an Edge/a List of Edges in the Graph with the given value. Can also be used to remove an Edge by choosing 'Nothing as a value'                          
replaceEdg :: Edge -> (Maybe Int) -> Graph -> Graph
replaceEdg e v (Graph i w) = Graph i (gUpdate [(e,v)] w)

replaceEdges :: [(Edge,(Maybe Int))] -> Graph -> Graph
replaceEdges xs (Graph i w) = Graph i (gUpdate xs w)


--Replaces a List of Paths in a Dist Graph with new Paths
replacePaths :: [Path] -> DistGraph -> DistGraph
replacePaths ps (Dist i d) = let ps' = [(e,(Path e w p)) | (Path e w p) <- ps] in (Dist i (d // ps'))


--TODO: Update together with new Weight Types
gUpdate :: [(Edge,(Maybe Int))] -> Weights -> Weights
gUpdate [(e,v)] (SimpleW f) = let f' e' = if (e' == e) then v else f e' in SimpleW f'
gUpdate xs (ArrayW wa)      = ArrayW (wa // xs)
gUpdate (x:xs) w            = gUpdate xs (gUpdate [x] w)








--Floyd Warshall Algorithm returns a Graph denoting shortest distances between pairs of nodes
floydWarshall :: Graph -> DistGraph
floydWarshall g = flwaRoutine 1 (convertToDist g)


--Inner Routine of the Floyd Warshall Algorithm. Notice that all objects are immutable so you can have the original graph as well as a graph where every edge is the distance between two nodes at the same time
flwaRoutine :: Int -> DistGraph -> DistGraph
flwaRoutine x g = if x > s then g else flwaRoutine (x+1) (replacePaths (checkDistViaX allPairs x g) g) where s = gSizeP g
                                                                                                             allPairs = [(x,y) | x <- [1..s], y <- [1..s]]


--For a List of Node Pairs, it checks if the path via node x is shorter and if it is returns the Path with the new distance. TODO: Catch invalid Integers
checkDistViaX :: [(Int,Int)] -> Int -> DistGraph -> [Path]
checkDistViaX [] _ _         = []
checkDistViaX ((a,b):xs) x g = let (Path _ wa ax) = findP (a,x) g
                                   (Path _ wb xb) = findP (x,b) g
                                   newW = mayplus wa wb
                               in if mayshorter newW (dist (findP (a,b) g)) then (Path (a,b) newW (ax ++ xb)) : checkDistViaX xs x g else checkDistViaX xs x g


                               
                               
--Returns the shorter of two Paths
shorterPath :: Path -> Path -> Path
shorterPath (Path e l el) (Path e' l' el') = if l <= l' then (Path e l el) else (Path e' l' el')
                               
                               
                               
                               
                               
--TODO: Move to Util
mayplus :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
mayplus Nothing b = Nothing
mayplus a Nothing = Nothing
mayplus (Just a) (Just b) = Just (a+b)


--TODO: Move to Util
mayshorter :: (Maybe Int) -> (Maybe Int) -> Bool
mayshorter Nothing b         = False
mayshorter a Nothing         = True
mayshorter (Just a) (Just b) =  a < b