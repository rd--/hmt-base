-- | Graph types.
module Music.Theory.Graph.Type where

import Data.Bifunctor {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}

import qualified Data.Graph as Graph {- containers -}

import qualified Music.Theory.List as T {- hmt-base -}

-- * Vertices

v_is_normal :: [Int] -> Maybe Int
v_is_normal v = let k = length v in if v == [0 .. k - 1] then Just k else Nothing

v_is_normal_err :: [Int] -> Int
v_is_normal_err = fromMaybe (error "v_is_normal?") . v_is_normal

-- * Edge

-- | Un-directed edge equality.
--
-- > e_eq_undir (0,1) (1,0) == True
e_eq_undir :: Eq t => (t,t) -> (t,t) -> Bool
e_eq_undir e0 e1 =
  let swap (i,j) = (j,i)
  in e0 == e1 || e0 == swap e1

-- | Sort edge.
--
-- > map e_sort [(0,1),(1,0)] == [(0,1),(0,1)]
e_sort :: Ord t => (t, t) -> (t, t)
e_sort (i,j) = (min i j,max i j)

-- * (vertices,edges) graph

-- | (vertices,edges)
type Gr t = ([t],[(t,t)])

-- | 'Gr' is a functor.
gr_map :: (t -> u) -> Gr t -> Gr u
gr_map f (v,e) = (map f v,map (bimap f f) e)

-- | (|V|,|E|)
gr_degree :: Gr t -> (Int,Int)
gr_degree (v,e) = (length v,length e)

-- | Re-label graph given table.
gr_relabel :: Eq t => [(t,u)] -> Gr t -> Gr u
gr_relabel tbl (v,e) =
  let get z = T.lookup_err z tbl
  in (map get v,map (bimap get get) e)

-- | If (i,j) and (j,i) are both in E delete (j,i) where i < j.
gr_mk_undir :: Ord t => Gr t -> Gr t
gr_mk_undir (v,e) = (v,nub (sort (map e_sort e)))

-- | List of E to G, derives V from E.
eset_to_gr :: Ord t => [(t,t)] -> Gr t
eset_to_gr e =
  let v = sort (nub (concatMap (\(i,j) -> [i,j]) e))
  in (v,e)

-- | Sort v and e.
gr_sort :: Ord t => Gr t -> Gr t
gr_sort (v,e) = (sort v,sort e)

-- | Complete k-graph (un-directed) given list of vertices
--
-- > gr_complete_graph "xyz" == ("xyz",[('x','y'),('x','z'),('y','z')])
gr_complete_graph :: Ord t => [t] -> Gr t
gr_complete_graph v = let e = [(i,j) | i <- v,j <- v,i < j] in (v,e)

-- * Int graph

-- | 'Gr' of 'Int'
type G = Gr Int

-- | Simple text representation of 'G'.  Requires (and checks) that vertices are (0 .. |v|-1).
--   The first line is the number of vertices, following lines are edges.
g_to_text :: G -> String
g_to_text (v,e) =
  let k = v_is_normal_err v
      f (i,j) = unwords (map show [i,j])
  in unlines (show k : map f e)

-- | 'Graph.Graph' to 'G'.
graph_to_g :: Graph.Graph -> G
graph_to_g gr = (Graph.vertices gr,Graph.edges gr)

-- | 'G' to 'Graph.Graph'
--
-- > g = ([0,1,2],[(0,1),(0,2),(1,2)])
-- > g == gr_sort (graph_to_g (g_to_graph g))
g_to_graph :: G -> Graph.Graph
g_to_graph (v,e) = Graph.buildG (minimum v,maximum v) e

-- | Unlabel graph, make table.
--
-- > gr_unlabel ("xyz",[('x','y'),('x','z')]) == (([0,1,2],[(0,1),(0,2)]),[(0,'x'),(1,'y'),(2,'z')])
gr_unlabel :: Eq t => Gr t -> (G,[(Int,t)])
gr_unlabel (v1,e1) =
  let n = length v1
      v2 = [0 .. n - 1]
      tbl = zip v2 v1
      get k = T.reverse_lookup_err k tbl
      e2 = map (bimap get get) e1
  in ((v2,e2),tbl)

-- | 'fst' of 'gr_unlabel'
gr_to_g :: Eq t => Gr t -> G
gr_to_g = fst . gr_unlabel

-- | 'g_to_graph' of 'gr_unlabel'.
--
-- > gr = ("abc",[('a','b'),('a','c'),('b','c')])
-- > (g,tbl) = gr_to_graph gr
gr_to_graph :: Eq t => Gr t -> (Graph.Graph,[(Int,t)])
gr_to_graph gr =
  let ((v,e),tbl) = gr_unlabel gr
  in (Graph.buildG (0,length v - 1) e,tbl)

-- | Complete k-graph (un-directed).
--
-- > g_complete_graph 3 == ([0,1,2],[(0,1),(0,2),(1,2)])
g_complete_graph :: Int -> G
g_complete_graph k = gr_complete_graph [0 .. k - 1]

-- * Edg = edge list (zero-indexed)

-- | ((|V|,|E|),[E])
type Edg = ((Int,Int), [(Int,Int)])

-- | Requires (and checks) that vertices are (0 .. |v| - 1).
g_to_edg :: G -> Edg
g_to_edg (v,e) = ((v_is_normal_err v,length e),e)

-- | Requires (but does not check) that vertices of 'Edg' are all in (0,|v| - 1).
edg_to_g :: Edg -> G
edg_to_g ((nv,ne),e) =
  let v = [0 .. nv - 1]
  in if ne /= length e
     then error (show ("edg_to_g",nv,ne,length e))
     else (v,e)

-- | Parse Edg as printed by nauty-listg.
edg_parse :: [String] -> Edg
edg_parse ln =
  let parse_int_list = map read . words
      parse_int_pairs = T.adj2 2 . parse_int_list
      parse_int_pair = T.unlist1_err . parse_int_pairs
  in case ln of
       [m,e] -> (parse_int_pair m,parse_int_pairs e)
       _ -> error "edg_parse"

-- * Adjacencies

-- | Adjacency list [(left-hand-side,[right-hand-side])]
type Adj t = [(t,[t])]

-- | 'Adj' to edge set.
adj_to_eset :: Ord t => Adj t -> [(t,t)]
adj_to_eset = concatMap (\(i,j) -> zip (repeat i) j)

-- | 'Adj' to 'Gr'
adj_to_gr :: Ord t => Adj t -> Gr t
adj_to_gr = eset_to_gr . adj_to_eset

-- | 'Gr' to 'Adj' (selection-function)
gr_to_adj :: Ord t => (t -> (t,t) -> Maybe t) -> Gr t -> Adj t
gr_to_adj sel_f (v,e) =
  let f k = (k,sort (mapMaybe (sel_f k) e))
  in filter (\(_,a) -> a /= []) (map f v)

-- | 'Gr' to 'Adj' (directed)
--
-- > g = ([0,1,2,3],[(0,1),(2,1),(0,3),(3,0)])
-- > r = [(0,[1,3]),(2,[1]),(3,[0])]
-- > gr_to_adj_dir g == r
gr_to_adj_dir :: Ord t => Gr t -> Adj t
gr_to_adj_dir =
  let sel_f k (i,j) = if i == k then Just j else Nothing
  in gr_to_adj sel_f

-- | 'Gr' to 'Adj' (un-directed)
--
-- > g = ([0,1,2,3],[(0,1),(2,1),(0,3),(3,0)])
-- > gr_to_adj_undir g == [(0,[1,3,3]),(1,[2])]
gr_to_adj_undir :: Ord t => Gr t -> Adj t
gr_to_adj_undir =
  let sel_f k (i,j) =
        if i == k && j >= k
        then Just j
        else if j == k && i >= k
             then Just i
             else Nothing
  in gr_to_adj sel_f

-- | Adjacency matrix, (|v|,mtx)
type Adj_Mtx t = (Int,[[t]])

{- | Edg to Adj_Mtx for un-directed graph.

> e = ((4,3),[(0,3),(1,3),(2,3)])
> edg_to_adj_mtx_undir (0,1) e == (4,[[0,0,0,1],[0,0,0,1],[0,0,0,1],[1,1,1,0]])

> e = ((4,4),[(0,1),(0,3),(1,2),(2,3)])
> edg_to_adj_mtx_undir (0,1) e == (4,[[0,1,0,1],[1,0,1,0],[0,1,0,1],[1,0,1,0]])

-}
edg_to_adj_mtx_undir :: (t,t) -> Edg -> Adj_Mtx t
edg_to_adj_mtx_undir (false,true) ((nv,_ne),e) =
  let v = [0 .. nv - 1]
      f i j = case find (e_eq_undir (i,j)) e of
                Nothing -> false
                _ -> true
  in (nv,map (\i -> map (f i) v) v)

-- | 'edg_to_adj_mtx_undir' of 'g_to_edg'
g_to_adj_mtx_undir :: (t,t) -> G -> Adj_Mtx t
g_to_adj_mtx_undir o = edg_to_adj_mtx_undir o . g_to_edg

-- | Lookup 'Adj_Mtx' to find connected vertices.
adj_mtx_con :: Eq t => (t,t) -> Adj_Mtx t -> Int -> [Int]
adj_mtx_con (false,true) (_,mx) e =
  let f i j = if i == true then Just j else if i == false then Nothing else error "adj_mtx_con?"
  in catMaybes (zipWith f (mx !! e) [0..])

-- * Labels

-- | Labelled graph, distinct vertex and edge labels.
type Lbl_Gr v v_lbl e_lbl = ([(v,v_lbl)],[((v,v),e_lbl)])

-- | 'Lbl_Gr' of 'Int'
type Lbl v e = Lbl_Gr Int v e

-- | 'Lbl' with () edge labels.
type Lbl_ v = Lbl v ()

-- | Number of vertices and edges.
lbl_degree :: Lbl v e -> (Int,Int)
lbl_degree (v,e) = (length v,length e)

-- | Apply /v/ at vertex labels and /e/ at edge labels.
lbl_bimap :: (v -> v') -> (e -> e') -> Lbl v e -> Lbl v' e'
lbl_bimap v_f e_f (v,e) = (map (fmap v_f) v,map (fmap e_f) e)

-- | Merge two 'Lbl' graphs, do not share vertices, vertex indices at /g1/ are stable.
lbl_merge :: Lbl v e -> Lbl v e -> Lbl v e
lbl_merge (v1,e1) (v2,e2) =
  let m = maximum (map fst v1) + 1
      v3 = map (\(i,j) -> (i + m,j)) v2
      e3 = map (\((i,j),k) -> ((i + m,j + m),k)) e2
  in (v1 ++ v3,e1 ++ e3)

-- | 'foldl1' of 'lbl_merge'
lbl_merge_seq :: [Lbl v e] -> Lbl v e
lbl_merge_seq = foldl1 lbl_merge

-- | Re-write graph so vertex indices are (0 .. n-1) and vertex labels are unique.
lbl_canonical :: (Eq v,Ord v) => Lbl v e -> Lbl v e
lbl_canonical (v1,e1) =
  let v2 = zip [0..] (nub (map snd v1))
      reix i = T.reverse_lookup_err (T.lookup_err i v1) v2
      e2 = map (\((i,j),k) -> ((reix i,reix j),k)) e1
  in (v2,e2)

-- | Re-write edges so that vertex indices are ascending.
lbl_undir :: Lbl v e -> Lbl v e
lbl_undir (v,e) = (v,map (\((i,j),k) -> ((min i j,max i j),k)) e)

-- | 'Lbl' path graph.
lbl_path_graph :: [x] -> Lbl_ x
lbl_path_graph v =
  let n = length v - 1
  in (zip [0 .. n] v
     ,zip (zip [0 .. n - 1] [1 .. n]) (repeat ()))

-- | 'Lbl' complete graph (undirected, no self-edges)
lbl_complete_graph :: [x] -> Lbl_ x
lbl_complete_graph v =
  let n = length v - 1
      u = [0 .. n]
  in (zip u v
     ,zip [(i,j) | i <- u, j <- u, i < j] (repeat ()))

-- | Lookup vertex label with default value.
v_label :: v -> Lbl v e -> Int -> v
v_label def (tbl,_) v = fromMaybe def (lookup v tbl)

-- | 'v_label' with 'error' as default.
v_label_err :: Lbl v e -> Int -> v
v_label_err = v_label (error "v_label")

-- | Lookup edge label with default value.
e_label :: e -> Lbl v e -> (Int,Int) -> e
e_label def (_,tbl) e = fromMaybe def (lookup e tbl)

-- | 'e_label' with 'error' as default.
e_label_err :: Lbl v e -> (Int,Int) -> e
e_label_err = e_label (error "e_label")

-- | Convert from 'Lbl_Gr' to 'Lbl'
lbl_gr_to_lbl :: Eq v => Lbl_Gr v v_lbl e_lbl -> Lbl v_lbl e_lbl
lbl_gr_to_lbl (v,e) =
  let n = length v
      v' = [0 .. n - 1]
      tbl = zip v' (map fst v)
      get k = T.reverse_lookup_err k tbl
      e' = map (\((p,q),r) -> ((get p,get q),r)) e
  in (zip v' (map snd v),e')

-- | Convert from 'Gr' to 'Lbl'.
--
-- > gr_to_lbl ("ab",[('a','b')]) == ([(0,'a'),(1,'b')],[((0,1),('a','b'))])
gr_to_lbl :: Eq t => Gr t -> Lbl t (t,t)
gr_to_lbl (v,e) = lbl_gr_to_lbl (zip v v,zip e e)

-- | Delete edge labels from 'Lbl', replacing with '()'
lbl_delete_edge_labels :: Lbl v e -> Lbl_ v
lbl_delete_edge_labels (v,e) = (v,map (\(x,_) -> (x,())) e)

-- | 'lbl_delete_edge_labels' of 'gr_to_lbl'
gr_to_lbl_ :: Eq t => Gr t -> Lbl_ t
gr_to_lbl_ = lbl_delete_edge_labels . gr_to_lbl

-- | Construct Lbl from set of E, derives V from E.
eset_to_lbl :: Ord t => [(t,t)] -> Lbl_ t
eset_to_lbl e =
  let v = nub (sort (concatMap (\(i,j) -> [i,j]) e))
      get_ix z = fromMaybe (error "eset_to_lbl") (elemIndex z v)
  in (zip [0..] v, map (\(i,j) -> ((get_ix i,get_ix j),())) e)

-- | Unlabel 'Lbl' graph.
lbl_to_g :: Lbl v e -> G
lbl_to_g (v,e) = (map fst v,map fst e)
