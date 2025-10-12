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
    getVertices,
    contractGraph,
    splitNodesIndegOne,
    getZipper,
    moveZipper,
    changeAtZipper,
    fromZipper,
    Zipper (),
    getAtZipper,
    getZipperVal,
    getZipperArcs,
    getZipperCurVert
) where


import qualified Data.IntMap as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import Control.Arrow (Arrow(second, first))
import Data.Tuple (swap)
import Utils


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


getVal :: Vertex -> Graph v e -> v
getVal (Vertex v) (Graph m) = snd3 $ m M.! v

getVertices :: Graph v e -> [Vertex] 
getVertices (Graph m) = map Vertex $ M.keys m

removeVertex :: Eq e => Vertex -> Graph v e -> Graph v e
removeVertex vert@(Vertex v) g@(Graph m) = 
    let ins = map (\(other, edgeval) -> (other, edgeval, vert) ) $ getInArcs vert g
        outs = map (\(other, edgeval) -> (vert, edgeval, other) ) $ getArcs vert g
        (Graph m') = foldr (uncurry3 removeArc) g $ ins ++ outs  
    in Graph $ M.delete v m'

-- | get outgoing arcs
getArcs :: Vertex -> Graph v e -> [(Vertex, e)]
getArcs (Vertex v) (Graph m) = map (first Vertex) $ thd3 $ m M.! v


getInArcs :: Vertex -> Graph v e -> [(Vertex, e)]
getInArcs (Vertex v) (Graph m) = map (first Vertex) $ fst3 $ m M.! v

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


indegree :: Vertex -> Graph v e -> Int
indegree (Vertex v) (Graph m) = length . fst3 $ m M.! v
outdegree :: Vertex -> Graph v e -> Int
outdegree (Vertex v) (Graph m) = length . fst3 $ m M.! v


-- | contracts entire graph. If a node has indegree=outdegree, then fuses the node with 
--   next node. Will return new graph, and the node which includes startnode
--   Klopt niet helemaal de uitleg van hoe contraction werkt, TODO
contractGraph :: Eq e => (v -> v -> v) -> Vertex -> Graph v e -> (Vertex, Graph v e)
contractGraph f startnode graph = 
    let -- make sure startnode gets done first, to ensure it doesn't get removed
        -- in the contraction step for another vertex, and we lose the reference to it
        (newstartnode, graph') = contractNode f startnode graph
        -- next do all other vertices, we don't need to remove the startnode because 
        -- it is ignored in contractNode because it has been deleted already (if contractable)
        verts = getVertices graph
        graph'' = foldr (snd .: contractNode f) graph' verts
    in (newstartnode, graph'')

contractNode :: Eq e => (v -> v -> v) -> Vertex -> Graph v e -> (Vertex, Graph v e)
contractNode f vert@(Vertex v) g@(Graph m) | isNothing (M.lookup v m) = (vert, g) 
            -- als vertex al door eerde contraction is verwijderd, dan doen we niets meer.
contractNode f vert@(Vertex v) g@(Graph m) =
    let outdeggood = outdegree vert g == 1
        nextvert = fst . head $ getArcs vert g
        nextgood = indegree nextvert g == 1
    in if outdeggood && nextgood 
        then
            let curval = getVal vert g
                nextval = getVal nextvert g
                g' = removeVertex vert $ removeVertex nextvert g
                (newvert, g'') = insertVertex (curval `f` nextval) g'

                ins = map (\(other, edgeval) -> (other, newvert, edgeval) ) $ getInArcs vert g
                outs = map (\(other, edgeval) -> (newvert, other, edgeval) ) $ getArcs nextvert g
                
                g''' = foldr (uncurry3 insertArc) g'' $ ins ++ outs
            in (newvert, g''')
        else (vert, g)


    
splitNodesIndegOne :: Eq e => Graph v e -> Graph v e
splitNodesIndegOne g = 
    let nodes = getVertices g
    in foldr splitNode g nodes

splitNode :: Eq e => Vertex -> Graph v e -> Graph v e
splitNode v g =
    let ins = getInArcs v g
        outs = getArcs v g
        curval = getVal v g

        copyNode (prednode, edgeval) graph = 
            let graph' = removeArc prednode edgeval v graph
                (newvert, graph'') = insertVertex curval graph'
                graph''' = insertArc prednode newvert edgeval graph'' 
            in foldr (uncurry3 insertArc) graph''' $ map (\(nextval, nextedgeval) -> (newvert, nextval, nextedgeval)) outs

    in foldr copyNode g ins


-- TODO dit alles naar eigen modules
data Zipper v e a = Zipper { 
    zipperGraph :: Graph v e, 
    zipperPos :: Vertex,
    zipperData :: a 
}

getZipperCurVert :: Zipper v e a -> Vertex
getZipperCurVert = zipperPos

getZipper :: Vertex -> a -> Graph v e -> Zipper v e a
getZipper v e g  = Zipper g v e

getAtZipper :: Zipper v e a -> v
getAtZipper (Zipper g p _) = getVal p g

fromZipper :: Zipper v e a -> (Graph v e, Vertex, a)
fromZipper (Zipper g p a) = (g,p,a)

moveZipper :: Vertex -> Zipper v e a -> Zipper v e a
moveZipper v (Zipper g p a) = (Zipper g v a)

changeAtZipper :: (v -> v) -> Zipper v e a -> Zipper v e a
changeAtZipper f (Zipper (Graph m) vert@(Vertex v) a) = Zipper (Graph $ M.adjust (second3 f) v m) vert a

instance Functor (Zipper v e) where
  fmap :: (a -> b) -> Zipper v e a -> Zipper v e b
  fmap f (Zipper g v a) = Zipper g v (f a)

getZipperVal :: Zipper v e a -> a
getZipperVal (Zipper _ _ a) = a

getZipperArcs :: Zipper v e a -> [(Vertex, e)]
getZipperArcs (Zipper g v a) = getArcs v g