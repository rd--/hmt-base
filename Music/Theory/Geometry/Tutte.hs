{- | Tutte, W. T. (1963), "How to draw a graph",
  Proceedings of the London Mathematical Society, 13: 743–767.
-}
module Music.Theory.Geometry.Tutte where

import Data.Bifunctor {- base -}

import Music.Theory.Geometry.Functions {- hmt-base -}
import Music.Theory.Geometry.Vector {- hmt-base -}

import qualified Music.Theory.Geometry.Obj as Obj {- hmt-base -}
import qualified Music.Theory.Geometry.Off as Off {- hmt-base -}
import qualified Music.Theory.Graph.Type as Graph {- hmt-base -}
import qualified Music.Theory.Image.Svg as Svg {- hmt-base -}
import qualified Music.Theory.Json as Json {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Math as Math {- hmt-base -}

{- | /k/ equally spaced points on unit circle

>>> map v2_round (v_on_unit_circle 4)
[(1,0),(0,1),(-1,0),(0,-1)]
-}
v_on_unit_circle :: Int -> [V2 Double]
v_on_unit_circle k =
  let i = two_pi / fromIntegral k
  in map (\ph -> polar_to_rectangular (1, ph)) (take k [0, i ..])

{- | Vertex locations. [(Vertex,Coordinate)]
Vertices are zero indexed.
-}
type V_Loc = [(Int, V2 Double)]

{- | k = n-vertices, fc = face

>>> let k = 8
>>> let fc = [0,1,2,3]
>>> let pp = map (\(i, j) -> (i, v2_round j))
>>> pp (v_init_loc k fc)
[(0,(1,0)),(1,(0,1)),(2,(-1,0)),(3,(0,-1)),(4,(0,0)),(5,(0,0)),(6,(0,0)),(7,(0,0))]
-}
v_init_loc :: Int -> [Int] -> V_Loc
v_init_loc k fc =
  let fc_v = zip fc (v_on_unit_circle (length fc))
      sel i = case lookup i fc_v of
        Just j -> (i, j)
        _ -> (i, (0, 0))
  in map sel [0 .. k - 1]

-- | 'v2_centroid' of indexed 'V_Loc'
v_loc_centre :: V_Loc -> [Int] -> V2 Double
v_loc_centre v e = v2_centroid (map (`List.lookup_err` v) e)

-- | adj = adjacency-matrix, fc = face, v = vertices-loc
tutte_step :: Graph.Adj_Mtx Int -> [Int] -> V_Loc -> V_Loc
tutte_step adj fc v =
  let f (i, j) =
        if i `elem` fc
          then (i, j)
          else (i, v_loc_centre v (Graph.adj_mtx_con (0, 1) adj i))
  in map f v

-- | Generate sequence of Tuttes given graph, face list and outer face index.
tutte_gen :: Graph.G -> [[Int]] -> Int -> [V_Loc]
tutte_gen (v, e) fc i =
  let k = length v
      adj = Graph.edg_to_adj_mtx_undir (0, 1) (Graph.g_to_edg (v, e))
      fc_i = fc !! i
      v0 = v_init_loc k fc_i
  in iterate (tutte_step adj fc_i) v0

-- | 'tutte_gen' of Off3
tutte_gen_off3 :: Off.Off3 Double -> Int -> ([V_Loc], [V2 Int])
tutte_gen_off3 o i =
  let (v, e) = Graph.lbl_to_g (Off.off_graph o)
      ((_, _), (_, fc)) = o
  in (tutte_gen (v, e) (map snd fc) i, e)

-- | Store tutte to Obj file.
tutte_obj :: FilePath -> V_Loc -> [[Int]] -> IO ()
tutte_obj fn v f = do
  let f' = map (\x -> ('f', x)) f
      add_z (x, y) = (x, y, 0)
  Obj.obj_store 4 fn (map (add_z . snd) v, f')

-- | Store tutte to Svg file. opt=(size, margin, precision)
tutte_svg :: (V2 Double, Double, Int) -> FilePath -> V_Loc -> [V2 Int] -> IO ()
tutte_svg opt fn v e = do
  let ix k = List.lookup_err k v
      ln = map (bimap ix ix) e
  Svg.svg_store_line_unif ((0, 0, 0), 1) fn opt ln

-- | Store tutte to Json file.
tutte_json :: FilePath -> V_Loc -> [[Int]] -> IO ()
tutte_json fn v f = do
  let n = Json.double . Math.round_to 0.0001
      v' = map Json.int [1 .. length v]
      c = map (\(_,(x,y)) -> Json.array [n x, n y]) v
      f' = map (\l -> Json.array (map Json.int l)) f
      o = Json.object
        [("vertexList", Json.array v')
        ,("vertexCoordinates", Json.array c)
        ,("faceList", Json.array f')]
  Json.writeFile fn o
