{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.DirGraph(
    Vertex (),
    Graph (),
    empty,
    insertVertex,
    insertArc,
    fromList,
    mapArcs,
    getVal,
    getArcs,
    allArcs,
    removeArc,
    mapVertices
) where


import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow (Arrow(second, first))
import Data.Tuple (swap)


newtype Vertex = Vertex Int deriving Show
-- index :: Vertex v -> Int
-- index (Vertex x _) = x

--data Edge v e = Edge e (Vertex v)

--newtype Graph v e = Graph (M.IntMap (v, M.IntMap e)) deriving Show
newtype Graph v e = Graph (M.IntMap (v, [(Int, e)])) deriving Show

empty :: Graph v e
empty = Graph $ M.empty

insertVertex :: v -> Graph v e -> (Vertex, Graph v e)
insertVertex v (Graph m) = 
    let index = 1 + (fromMaybe (-1) . (fmap fst) $ M.lookupMax m)
        graph = Graph $ M.insert index (v,[]) m
    in (Vertex index, graph)


mapVertices :: (Vertex -> v2) -> Graph v1 e1 -> Graph v2 e1
mapVertices f (Graph m) = Graph $ M.mapWithKey (\v (val, arcs) -> (f $ Vertex v, arcs) ) m


-- | Warning: if there is already an arc between these vertices, it is overwritten.
--   TODO: DIT KLOPT NIET, multi-arcs worden nou wel toegestaan. We gebruiken nu wel een [(int, edge)] layout dus dat is niet bepaald efficient mogelijk
insertArc :: Vertex -> Vertex -> e -> Graph v e -> Graph v e
insertArc (Vertex i1) (Vertex i2) edgeval (Graph m) = 
    Graph $ M.adjust (second $ ( (i2, edgeval) : )) i1 m

getVal :: Vertex -> Graph v e -> v
getVal (Vertex v) (Graph m) = fst $ m M.! v

getArcs :: Vertex -> Graph v e -> [(Vertex, e)]
getArcs (Vertex v) (Graph m) = map (first Vertex) $ snd $ m M.! v

allArcs :: Graph v e -> [(Vertex, e, Vertex)]
allArcs (Graph m) = concatMap (\(v1,(nodeval, arcs)) -> map (\(v2, edgeval) -> (Vertex v1, edgeval, Vertex v2)) arcs ) $ M.assocs m

mapArcs :: forall v e1 e2. (Vertex -> Vertex -> e1 -> e2) -> Graph v e1 -> Graph v e2
mapArcs f (Graph m) = 
    let mapf :: Int -> (v, [(Int, e1)]) -> (v, [(Int, e2)])
        mapf i1 (vertval, es) = (vertval, map (\(i2, edgeval) -> (i2, f (Vertex i1) (Vertex i2) edgeval)) es)
    in Graph $ M.mapWithKey mapf m

removeArc :: Eq e => Vertex -> e -> Vertex -> Graph v e -> Graph v e
removeArc (Vertex v1) e (Vertex v2) (Graph m) = Graph $  M.adjust (second $ filter ((==e) . snd)) v1 m


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

