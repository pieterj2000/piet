module Data.Graph () where


data Vertex a = Vertex {
    value :: a,
    edges :: [Vertex a]
} deriving Show


makeGraph :: [(Int, [Int])] -> [Vertex Int]
makeGraph input = nodes
    where
        nodes = doe edges input 
        edgesf (i, es) = filter (\v -> (value v) `elem` es) nodes
        edges = map edgesf input

        doe :: [[Vertex Int]] -> [(Int, [Int])] -> [Vertex Int]
        doe _ [] = []
        doe (e:es) ((huidig, _):rest) = (Vertex huidig e) : doe es rest