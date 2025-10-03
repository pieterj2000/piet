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
    mapVertices,
    allInArcs,
    getVertices
) where


import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Arrow (Arrow(second, first))
import Data.Tuple (swap)


newtype Vertex = Vertex Int deriving (Show, Eq) -- TODO dit iis niet type safe
-- index :: Vertex v -> Int
-- index (Vertex x _) = x

--data Edge v e = Edge e (Vertex v)

--newtype Graph v e = Graph (M.IntMap (v, M.IntMap e)) deriving Show
newtype Graph v e = Graph (M.IntMap ([(Int, e)], v, [(Int, e)])) deriving Show

empty :: Graph v e
empty = Graph $ M.empty

insertVertex :: v -> Graph v e -> (Vertex, Graph v e)
insertVertex v (Graph m) = 
    let index = 1 + (fromMaybe (-1) . (fmap fst) $ M.lookupMax m)
        graph = Graph $ M.insert index ([], v,[]) m
    in (Vertex index, graph)


mapVertices :: (Vertex -> v2) -> Graph v1 e1 -> Graph v2 e1
mapVertices f (Graph m) = Graph $ M.mapWithKey (\v (inarcs, val, arcs) -> (inarcs, f $ Vertex v, arcs) ) m


-- | Warning: if there is already an arc between these vertices, it is overwritten.
--   TODO: DIT KLOPT NIET, multi-arcs worden nou wel toegestaan. We gebruiken nu wel een [(int, edge)] layout dus dat is niet bepaald efficient mogelijk
insertArc :: Vertex -> Vertex -> e -> Graph v e -> Graph v e
insertArc (Vertex i1) (Vertex i2) edgeval (Graph m) = 
    Graph $ 
        M.adjust (\(inarcs, val, outarcs) -> ((i1, edgeval) : inarcs, val, outarcs)) i2 $
        M.adjust (\(inarcs, val, outarcs) -> (inarcs, val, (i2, edgeval) : outarcs)) i1 m

snd3 :: (a, b, c) -> b
snd3 (a,b,c) = b 
thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c
first3 :: (a -> d) -> (a,b,c) -> (d,b,c)
first3 f (a,b,c) = (f a,b,c)
third3 :: (c -> d) -> (a,b,c) -> (a,b,d)
third3 f (a,b,c) = (a,b,f c)

getVal :: Vertex -> Graph v e -> v
getVal (Vertex v) (Graph m) = snd3 $ m M.! v

getVertices :: Graph v e -> [Vertex] 
getVertices (Graph m) = map Vertex $ M.keys m

-- | get outgoing arcs
getArcs :: Vertex -> Graph v e -> [(Vertex, e)]
getArcs (Vertex v) (Graph m) = map (first Vertex) $ thd3 $ m M.! v

allArcs :: Graph v e -> [(Vertex, e, Vertex)]
allArcs (Graph m) = concatMap (\(v1,(inarcs, nodeval, outarcs)) -> map (\(v2, edgeval) -> (Vertex v1, edgeval, Vertex v2)) outarcs ) $ M.assocs m

allInArcs :: Graph v e -> [(Vertex, e, Vertex)]
allInArcs (Graph m) = concatMap (\(v2,(inarcs, nodeval, outarcs)) -> map (\(v1, edgeval) -> (Vertex v1, edgeval, Vertex v2)) inarcs ) $ M.assocs m

-- | Note: Function gets computed twice for each edge (as an arc for the outgoing vertex and incoming vertex), 
--   so the function shouldn't too heavy. TODO fix this.
mapArcs :: forall v e1 e2. (Vertex -> Vertex -> e1 -> e2) -> Graph v e1 -> Graph v e2
mapArcs f (Graph m) = 
    let fo i1 (i2,edgeval) = (i2, f (Vertex i1) (Vertex i2) edgeval)
        fi i2 (i1,edgeval) = (i1, f (Vertex i1) (Vertex i2) edgeval)
        mapf :: Int -> ([(Int, e1)], v, [(Int, e1)]) -> ([(Int, e2)], v, [(Int, e2)])
        mapf i (inarcs, vertval, outarcs) = (map (fi i) inarcs, vertval, map (fo i) outarcs)
    in Graph $ M.mapWithKey mapf m

-- | Note: Function gets computed twice for each edge (as an arc for the outgoing vertex and incoming vertex), 
--   so the function shouldn't too heavy. TODO fix this.
removeArc :: Eq e => Vertex -> e -> Vertex -> Graph v e -> Graph v e
removeArc (Vertex v1) e (Vertex v2) (Graph m) = 
    Graph $
    M.adjust (first3 $ filter (\(v,val) -> v /= v1 || val /= e)) v2 $
    M.adjust (third3 $ filter (\(v,val) -> v /= v2 || val /= e)) v1 m


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


contractGraph :: (v -> v -> vv) -> Vertex -> Graph v e -> Graph vv e
contractGraph f startnode (Graph m) = 
    let


    in undefined