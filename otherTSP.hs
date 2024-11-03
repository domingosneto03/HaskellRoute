import qualified Data.List
import qualified Data.Array as Array
import Data.Bits ((.&.), (.|.))
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.List (permutations, minimumBy, foldl', lookup)
import Data.Array (Array, listArray, (!), (//), bounds)
import GHC.Generics (Constructor(conFixity))
import Distribution.SPDX (LicenseId(Community_Spec_1_0))

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

cities :: RoadMap -> [City]
cities roadmap = rmDups [city | (c1, c2, _) <- roadmap, city <- [c1, c2]]


areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadmap city1 city2 = any checkAdj roadmap
  where checkAdj (c1, c2, _) = (city1 == c1 && city2 == c2) || (city1 == c2 && city2 == c1)

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing
distance ((c1, c2, d):xs) city1 city2
    | (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1) = Just d
    | otherwise = distance xs city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadmap city =
    let connections = filter (\(c1, c2, _) -> c1 == city || c2 == city) roadmap
    in [(if c1 == city then c2 else c1, d) | (c1, c2, d) <- connections]

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance roadmap path = let distances = zipWith (distance roadmap) path (tail path)
    in if all isJust distances  -- check if all distances are Just
       then Just (sum (map getFromJust distances))  -- sum all Just values
       else Nothing

rome :: RoadMap -> [City]
rome roadmap = let cities = concatMap (\(c1, c2, _) -> [c1, c2]) roadmap
                   citiesGrouped = Data.List.group (Data.List.sort cities)
                   cityCount = [(city, length group) | group <- citiesGrouped, let city = head group]
                   max = maximum (map snd cityCount)
               in [city | (city, count) <- cityCount, count == max]

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadmap =
    let allCities = cities roadmap
        startCity = head allCities
        reachable = dfs roadmap startCity []
    in length reachable == length allCities

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap startCity finalCity
    | startCity == finalCity = [[startCity]]
    | otherwise = dijkstra [(startCity, [startCity], 0)] [] Nothing where
        dijkstra [] _ _ = []
        dijkstra ((current, path, dist):queue) visited minDistance
            | current == finalCity =
                let isShortest = maybe True (dist <=) minDistance
                    newMinDist = if isShortest then Just dist else minDistance
                    pathsToReturn = ([path | isShortest])
                in pathsToReturn ++ dijkstra queue visited newMinDist
            | current `elem` visited = dijkstra queue visited minDistance
            | otherwise =
                let adj = adjacent roadmap current
                    new = queue ++ [(n, path ++ [n], dist + d) | (n, d) <- adj, n `notElem` visited]
                    sorted = Data.List.sortOn (\(_, _, d) -> d) new
                in dijkstra sorted (current : visited) minDistance


-- Travel Salesman Problem using Dynamic Programming
{- Stack error
travelSales :: RoadMap -> Path
travelSales roadmap =
    let citiesList = cities roadmap
        n = length citiesList
        cityIndex = zip citiesList [0..] -- Map city names to indexes
        distanceArray = createDistanceArray roadmap cityIndex n -- Create a distance matrix
        memo = Array.listArray ((0, 0), (2^n - 1, n - 1)) (repeat Nothing) -- Memoization table

        -- Recursive function with memoization
        tspRec :: Int -> Int -> Int
        tspRec mask position
            | mask == (2^n - 1) = distanceArray Array.! (position, 0) -- return to start
            | otherwise = case memo Array.! (mask, position) of
                Just cost -> cost -- return already computed cost
                Nothing -> let costs = [distanceArray Array.! (position, next) + tspRec (mask .|. (2^next)) next
                                        | next <- [0..n-1], (mask .&. (2^next)) == 0] -- valid next cities
                            in let minimumCost = minimum costs
                               in minimumCost `seq` (memo Array.// [((mask, position), Just minimumCost)]) `seq` minimumCost

        totalCost = tspRec 1 0 -- start from city 0 with only city 0 visited
        path = reconstructPath memo cityIndex distanceArray n -- reconstruct the path
    in if totalCost == maxBound then [] else path

-- Create a distance matrix for lookups
createDistanceArray :: RoadMap -> [(City, Int)] -> Int -> Array.Array (Int, Int) Distance
createDistanceArray roadmap cityIndex n =
    Array.listArray ((0, 0), (n-1, n-1))
    [if (c1, c2) `elem` edges || (c2, c1) `elem` edges then fromJust d else maxBound
     | c1 <- [0..n-1], c2 <- [0..n-1],
       let d = distance roadmap (fst (cityIndex !! c1)) (fst (cityIndex !! c2)),
       let edges = [(lookup a cityIndex, lookup b cityIndex) | (a, b, _) <- roadmap]]
    where
        lookup city indexed = case Data.List.lookup city indexed of Just i -> i; Nothing -> -1

-- Reconstruct the path after computing the cost
reconstructPath :: Array.Array (Int, Int) (Maybe Distance) -> [(City, Int)] -> Array.Array (Int, Int) Distance -> Int -> Path
reconstructPath memo cityIndex distanceArray n =
    let go mask position path
            | mask == (2^n - 1) = reverse (start:path) -- return to start
            | otherwise =
                let nextCities = [next | next <- [0..n-1], (mask .&. (2^next)) == 0]
                    costs = [distanceArray Array.! (position, next) + fromJust (memo Array.! (mask .|. (2^next), next)) | next <- nextCities]
                    nextPos = nextCities !! fst (minIndex costs)
                in go (mask .|. (2^nextPos)) nextPos (fst (cityIndex !! nextPos) : path)

        start = fst (head cityIndex)
        mask = 1 -- starting city has been visited
    in go mask 0 []

        where
            minIndex costs = Data.List.foldl' (\(i, m) (j, cost) -> if cost < m then (j, cost) else (i, m)) (0, maxBound) (zip [0..] costs)

-}

-- Brute Force Approach to find shortest possible path for the TSP
tspBruteForce :: RoadMap -> Path
tspBruteForce roadmap =
    case validPaths of
        []      -> [] -- No valid paths, so function return empty list
        paths -> minimumBy compareDistance paths -- Get the path with the least Distance
    where
        citiesList = cities roadmap
        allPaths = permutations citiesList -- Generate all possible city combinations
        validPaths = mapMaybe (closingPath roadmap) allPaths -- filter valid Tsp Paths (only keeps the valid closed paths)

        compareDistance city1 city2 = compare (pathDistance roadmap city1) (pathDistance roadmap city2)

-- Adds a starting city at the end of the path and check its validity
closingPath :: RoadMap -> Path -> Maybe Path
closingPath roadmap path
        | isValid = Just closedPath -- Return the closed path if valid
        | otherwise = Nothing
    where
        closedPath = path ++ [head path] -- add starting city to the end
        isValid = isJust (pathDistance roadmap closedPath) -- Check if distance if defined


-- only for groups of 3 people; groups of 2 people: do not edit this function

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