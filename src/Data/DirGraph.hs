{-# LANGUAGE InstanceSigs #-}
module Data.DirGraph(
    Vertex (),
    Graph (),
    empty,
    insertVertex,
    insertArc,
    fromList
) where


import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow (Arrow(second))


newtype Vertex = Vertex Int deriving Show
-- index :: Vertex v -> Int
-- index (Vertex x _) = x

--data Edge v e = Edge e (Vertex v)
data Arc v e = Arc e Int 

newtype Graph v e = Graph (M.IntMap (v, M.IntMap e)) deriving Show
--newtype Graph v e = Graph (M.IntMap [Arc v e])

empty :: Graph v e
empty = Graph $ M.empty

insertVertex :: v -> Graph v e -> (Vertex, Graph v e)
insertVertex v (Graph m) = 
    let index = 1 + (fromMaybe (-1) . (fmap fst) $ M.lookupMax m)
        graph = Graph $ M.insert index (v,M.empty) m
    in (Vertex index, graph)

-- | Warning: if there is already an arc between these vertices, it is overwritten.
insertArc :: Vertex -> Vertex -> e -> Graph v e -> Graph v e
insertArc (Vertex i1) (Vertex i2) edgeval (Graph m) = 
    Graph $ M.adjust (second $ M.insert i2 edgeval) i1 m


fromList :: Ord l => [(l,v)] -> [(l,l,e)] -> ( Graph v e, l -> Vertex )
fromList verts edges =
    let f :: Ord l => (l,v) -> (Graph v e, Map.Map l Vertex) -> (Graph v e, Map.Map l Vertex)
        f (l,v) (g,m) = (g', m')
            where   (vertex, g') = insertVertex v g
                    m' = Map.insert l vertex m
        (graph, labelmap) = foldr f (empty, Map.empty) verts

        f2 (i1,i2,e) g = insertArc (labelmap Map.! i1) (labelmap Map.! i2) e g
        graph' = foldr f2 graph edges
    in (graph', (labelmap Map.!))