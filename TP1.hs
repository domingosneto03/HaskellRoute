import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
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
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

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