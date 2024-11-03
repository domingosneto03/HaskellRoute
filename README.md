# feup-pfl-proj1

## Group Members
- Domingos Neto - up202108728 (50%)
- Mariana Conde - (50%)

## Tasks

### 1. cities : : RoadMap -> [City]
This function extracts all unique cities from a given `RoadMap`. By collecting both cities from each road and using a helper function `rmDups` to remove duplicates, `cities` returns a list of all cities in the roadmap.

### 2. areAdjacent : : RoadMap -> City -> City -> Bool
The `areAdjacent` function checks if two specified cities are directly connected by a road in the `RoadMap`. It iterates through the roads in the roadmap and returns `True` if any road connects the two cities, and `False` otherwise.

### 3. distance : : RoadMap -> City -> City -> Maybe Distance
This function returns the distance between two cities if they are directly connected by a road. It searches for a road in the `RoadMap` that connects the two cities and returns the distance wrapped in a `Just`. If no direct road is found, it returns `Nothing`.

### 4. adjacent : : RoadMap -> City -> [(City, Distance)]
The `adjacent` function returns a list of cities that are directly connected to a given city, along with the distance of each connection. It filters the `RoadMap` for all roads involving the given city and creates a list of neighboring cities and distances, providing a simple representation of the city's immediate connections.

### 5. pathDistance : : RoadMap -> Path -> Maybe Distance
This function calculates the total distance of a sequence of cities in the `RoadMap`. If all consecutive cities in the path are directly connected by roads, it returns the sum of those distances wrapped in a `Just`. If any two consecutive cities doesn't have a direct connection, it returns `Nothing`, indicating an invalid path.

### 6. rome : : RoadMap -> [City]
The `rome` function identifies cities with the highest number of direct connections (roads) in the `RoadMap`. It counts the occurrences of each city in all roads, groups cities by name, and finds the maximum count. The function then returns a list of cities with this maximum count.

### 7. isStronglyConnected : : RoadMap -> Bool
This function checks if the roadmap is "strongly connected," meaning all cities are reachable from each other. Starting from an arbitrary city, it uses depth-first search (`dfs`) to explore reachable cities. If all cities in the roadmap are reachable from the starting city, the function returns `True` and `False` otherwise.

## ShortestPath

### 8. shortestPath : : RoadMap -> City -> City -> [Path]

The `shortestPath` function finds all shortest paths between two cities in an undirected `RoadMap`. This function gathers all paths that share the minimal distance, as there may be multiple equivalent shortest paths in terms of distance which represents the weight of a graph.

---

#### Algorithm Description

The function employs a modified version of **Dijkstra’s Algorithm**, typically used to find the shortest path in a graph with weighted edges. 
It operates by exploring paths in order of their cumulative distance, always expanding the shortest known paths first. 
However, instead of stopping at the first shortest path to the destination, `shortestPath` continues exploring all paths to ensure all minimal paths are captured. 

#### Steps of the Algorithm

1. **Base Case**:
   - If the `startCity` is the same as the `finalCity`, it directly returns a path `[[startCity]]`, as this is trivially the shortest path.

2. **Initialization**:
   - The algorithm starts with a **priority queue** that is initialized with a tuple containing:
     - `startCity` as the current city,
     - `[startCity]` as the path traversed so far,
     - `0` as the total distance traveled from the starting city.
       
   - This queue will hold paths to explore, sorted by their cumulative distance to prioritize the shortest path expansions.

3. **Recursive Path Exploration**:
   - The function defines an internal helper, `dijkstra`, which processes paths in the queue in ascending order of their cumulative distance.
     
   - For each element dequeued (`current` city, `path`, and `distance`):
     - If `current` is the `finalCity`: 
       - It checks if this path’s distance is less than or equal to the minimum distance (`minDistance`) found so far.
       - If it is, the path is recorded as a shortest path.
       - `minDistance` is updated if this is the first shortest path found or if the new path has an equal minimum distance.
         
     - If `current` is not the `finalCity`:
       - The function iterates over each neighboring city of `current` (retrieved via `adjacent`) that hasn’t been visited in this path.
       - It appends these neighboring cities to `path`, calculates the new cumulative distance, and adds the new path to the queue.
       - The queue is sorted by distance after each update, ensuring that shorter paths are processed first.

4. **Path Collection**:
   - The algorithm continues exploring until all possible paths within the minimum distance are found.
     
   - Finally, it returns all paths that meet the shortest distance criterion.

---

#### Justification for Auxiliary Data Structures

1. **Priority Queue**:
   - The **priority queue** is implemented as a list, sorted by cumulative distance (`Data.List.sortOn` is used after each update).
     
   - A priority queue is essential for Dijkstra’s Algorithm, as it ensures the shortest paths are expanded first.

2. **Visited List**:
   - The **visited** list helps avoid redundant calculations by ensuring cities are not revisited within a single path exploration.
     
   - This reduces unnecessary recursive calls, especially in large, interconnected graphs, improving the function’s efficiency.

3. **Minimum Distance Tracker (`minDistance`)**:
   - A `Maybe Distance` (`minDistance`) variable tracks the shortest path distance found to `finalCity` so far. It’s initialized as `Nothing`, and updated upon discovering the first path to the destination.
     
   - This helps efficiently filter paths, only including those with the same minimal distance, thus optimizing the output to only include relevant shortest paths.

---

#### Code Walkthrough

```haskell
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadmap startCity finalCity
    | startCity == finalCity = [[startCity]]
    | otherwise = dijkstra [(startCity, [startCity], 0)] [] Nothing where
        -- Helper function to recursively explore paths
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

