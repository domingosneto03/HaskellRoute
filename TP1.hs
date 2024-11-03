import qualified Data.List
-- import qualified Data.Array
import qualified Data.Array as Arr
import qualified Data.Maybe as Maybe
import qualified Data.IntMap as IntMap
import Data.Bits (shiftL, (.&.), (.|.))


import Debug.Trace (trace, traceShow)

import GHC.Generics (Constructor(conFixity))
import Distribution.SPDX (LicenseId(Community_Spec_1_0))

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]


cities :: RoadMap -> [City] -- extract all unique cities from a roadmap
cities roadmap = rmDups [city | (c1, c2, _) <- roadmap, city <- [c1, c2]]


areAdjacent :: RoadMap -> City -> City -> Bool -- check if two cities are directly linked in the roadmap
areAdjacent roadmap city1 city2 = any checkAdj roadmap -- checkAdj checks if a pair of cities (c1, c2) matches (city1, city2) in either order.
  where checkAdj (c1, c2, _) = (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)

distance :: RoadMap -> City -> City -> Maybe Distance -- find the distance between two cities if they are directly connected, otherwise return Nothing
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) city1 city2
    | (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1) = Just d -- recursively searches through the roadmap and returns the distance
    | otherwise = distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)] -- list all cities directly connected to a given city along with their distances
adjacent roadmap city =
    let connections = filter (\(c1, c2, _) -> c1 == city || c2 == city) roadmap -- filters out all roads involving the given city
    in [(if c1 == city then c2 else c1, d) | (c1, c2, d) <- connections]

pathDistance :: RoadMap -> Path -> Maybe Distance -- total distance of a given path
pathDistance roadmap path = 
    let distances = zipWith (distance roadmap) path (tail path)
    in if all isJust distances  -- check if all distances are Just
       then Just (sum (map getFromJust distances))  -- sum all Just values
       else Nothing

rome :: RoadMap -> [City] -- identify the most connected cities in the roadmap
rome roadmap = 
    let cities = concatMap (\(c1, c2, _) -> [c1, c2]) roadmap
        citiesGrouped = Data.List.group (Data.List.sort cities)
        cityCount = [(city, length group) | group <- citiesGrouped, let city = head group]
        max = maximum (map snd cityCount)
    in [city | (city, count) <- cityCount, count == max]

isStronglyConnected :: RoadMap -> Bool -- check if all cities in the roadmap are reachable from any starting city
isStronglyConnected roadmap =
    let allCities = cities roadmap
        startCity = head allCities
        reachable = dfs roadmap startCity [] -- performs a dfs from a start city and verifies if all cities are reachable from that city.
    in length reachable == length allCities

shortestPath :: RoadMap -> City -> City -> [Path] -- find the shortest paths between two cities using Dijkstra's algorithm
shortestPath roadmap startCity finalCity
    | startCity == finalCity = [[startCity]]
    | otherwise = dijkstra [(startCity, [startCity], 0)] [] Nothing where
        dijkstra [] _ _ = []
        dijkstra ((current, path, dist):queue) visited minDistance
            | current == finalCity =
                let isShortest = maybe True (dist <=) minDistance
                    newMinDist = if isShortest then Just dist else minDistance
                    pathsToReturn = ([path | isShortest]) -- maintains a priority queue of paths to explore
                in pathsToReturn ++ dijkstra queue visited newMinDist
            | current `elem` visited = dijkstra queue visited minDistance
            | otherwise =
                let adj = adjacent roadmap current
                    new = queue ++ [(n, path ++ [n], dist + d) | (n, d) <- adj, n `notElem` visited]
                    sorted = Data.List.sortOn (\(_, _, d) -> d) new
                in dijkstra sorted (current : visited) minDistance



-- TSP Dynamic Programming
{- Wrong Path Output
travelSales :: RoadMap -> Path
travelSales roadmap
    | not (safeSC roadmap) = [] -- Return an empty path if graph is disconnected
    | otherwise =
        let allCities = cities roadmap -- gets all cities in roadmap
            totalCities = length allCities -- gets the total number of cities in roadmap
            maxCitiesVisitedMask = (1 `shiftL` totalCities) - 1 -- bit mask where all cities have been visited, e.g., totalCities = 5 -> mask = 11111

            -- Distance Array (stores distance between cities)
            distanceArray = Arr.array ((0,0), (totalCities - 1, totalCities -1))
                [((city1, city2), distance roadmap (allCities !! city1) (allCities !! city2))
                    | city1 <- [0..totalCities - 1], city2 <- [0..totalCities - 1]]  

            -- Memoization array to store minimum cost for each state (current city, already visited cities)
            initialMemoizationTable = Arr.array ((0,0), (totalCities - 1, maxCitiesVisitedMask))
                [((city1, mask), Nothing) | city1 <- [0..totalCities-1], mask <- [0..maxCitiesVisitedMask]]
            -- Array that stores the next city to visit for path Reconstruction
            initialPathMemo = Arr.array ((0,0), (totalCities - 1, maxCitiesVisitedMask)) 
                [((city1, mask), Nothing) | city1 <- [0..totalCities-1], mask <- [0..maxCitiesVisitedMask]]
    
            -- Recursive Function to find minimum cost
            tspRecMinCost :: Int -> Int -> Arr.Array (Int, Int) (Maybe Distance) -> (Distance, Arr.Array (Int, Int) (Maybe Distance))
            tspRecMinCost position mask memoizationTable
                -- if mask is the same as when all cities visited, return to starting city
                | mask == maxCitiesVisitedMask = 
                    let returnDistance = safeDistance distanceArray (position, 0)
                    in (returnDistance, memoizationTable)
                | Maybe.isNothing (memoizationTable Arr.! (position, mask)) =
                    traceShow ("Exploring from Position: " ++ show position ++ ", Mask: " ++ show mask) $ 
                    let costOptions = 
                            [ (safeDistance distanceArray (position, nextCity) + minimumCost, nextCity)
                            | nextCity <- [0..totalCities - 1]
                            , (mask .&. (1 `shiftL` nextCity)) == 0 -- ensure nextCity is not visited
                            , let (minimumCost, _) = tspRecMinCost nextCity (mask .|. (1 `shiftL` nextCity)) memoizationTable
                            , minimumCost < maxBound -- filter out invalid paths
                            ]
                    in if null costOptions
                        then (maxBound, memoizationTable)
                        else let (minimumCost, nextCity) = minimum costOptions
                            in (minimumCost, memoizationTable Arr.// [((position, mask), Just minimumCost)])
                | otherwise = 
                    let cachedResult = Maybe.fromJust (memoizationTable Arr.! (position, mask))
                    in (cachedResult, memoizationTable)

            -- Start TSP from the first City
            startingCity = 0
            initialMask = 1 `shiftL` startingCity
            (minimumCost, finalMemoizationTable) = tspRecMinCost startingCity initialMask initialMemoizationTable
        
            -- Reconstruct the path
            pathReconstruction :: Int -> Int -> Arr.Array (Int, Int) (Maybe Distance) -> Path
            pathReconstruction position mask pathMemo
                | mask == maxCitiesVisitedMask = (allCities !! position) : [head allCities] -- return to starting city
                | otherwise =
                    case pathMemo Arr.! (position, mask) of
                        Just nextCity -> (allCities !! position) : pathReconstruction nextCity (mask .|. (1 `shiftL` nextCity)) pathMemo
                        Nothing       -> [] -- Return empty if there is no path
        
        in if minimumCost == maxBound 
            then [] 
            else pathReconstruction startingCity initialMask finalMemoizationTable

-}



-- Brute Force Approach to find shortest possible path for the TSP
tspBruteForce :: RoadMap -> Path
tspBruteForce roadmap =
    case validPaths of
        []      -> [] -- No valid paths, so function return empty list
        paths -> Data.List.minimumBy compareDistance paths -- Get the path with the least Distance
    where
        citiesList = cities roadmap
        allPaths = Data.List.permutations citiesList -- Generate all possible city combinations
        validPaths = Maybe.mapMaybe (closingPath roadmap) allPaths -- filter valid Tsp Paths (only keeps the valid closed paths)

        compareDistance city1 city2 = compare (pathDistance roadmap city1) (pathDistance roadmap city2)

-- Adds a starting city at the end of the path and check its validity
closingPath :: RoadMap -> Path -> Maybe Path
closingPath roadmap path
        | isValid = Just closedPath -- Return the closed path if valid
        | otherwise = Nothing
    where
        closedPath = path ++ [head path] -- add starting city to the end
        isValid = isJust (pathDistance roadmap closedPath) -- Check if distance if defined


-- aux functions

rmDups :: (Eq a) => [a] -> [a] -- removes duplicates from a list
rmDups [] = []
rmDups (x:xs) = x : rmDups (filter (/= x) xs)

getFromJust :: Maybe a -> a -- get the value from a Just
getFromJust (Just x) = x
getFromJust Nothing  = error "getFromJust: Nothing value"

isJust :: Maybe a -> Bool -- check if a Maybe value is Just
isJust (Just _) = True
isJust Nothing  = False

dfs :: RoadMap -> City -> [City] -> [City] -- depht first search through the graph
dfs roadmap city visited =
    let newVisited = city : visited
        neighbors = map fst (adjacent roadmap city)
    in foldr (\n acc -> if n `elem` visited then acc else dfs roadmap n newVisited) newVisited neighbors


safeDistance :: Arr.Array (Int, Int) (Maybe Distance) -> (Int, Int) -> Distance -- safe distance function that defaults to infinity if no route exists
safeDistance arr (city1, city2) =
    let (lowerBound, upperBound) = Arr.bounds arr -- Get the bounds of the array
        (lowerRow, lowerCol) = lowerBound -- Deconstruct the lower bound
        (upperRow, upperCol) = upperBound -- Deconstruct the upper bound
    in if city1 < lowerRow || city2 < lowerCol || city1 > upperRow || city2 > upperCol
        then maxBound
        else case arr Arr.! (city1, city2) of
            Nothing -> maxBound
            Just d  -> d

        
safeSC :: RoadMap -> Bool
safeSC roadmap = 
    let allCities = cities roadmap
        reachableFrom city = dfs roadmap city []
        reachableCities = map reachableFrom allCities
    in all (\visited -> length visited == length allCities) reachableCities

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

-- personal graphs
gTest4 :: RoadMap -- test for shortest path
gTest4 = [("0","1",1),("1","3",2), ("0","2",1), ("2","4",1), ("4","3",1)]


-- added
gTest5 :: RoadMap
gTest5 = [("0", "1", 1), ("1", "2", 1), ("2", "3", 1), ("3", "0", 1)]
