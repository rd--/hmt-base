{- | Off - Reader and writer for a subset of the Off 3d Object File Format

                           OFF - A 3D Object File Format

                                   Randi J. Rost
                                  6-November-1986
                              Updated 12-October-1989

                           Digital Equipment Corporation
                          Workstation Systems Engineering
                                 100 Hamilton Ave.
                                Palo Alto, Ca. 94301


GeomView: <http://www.geomview.org/docs/html/OFF.html>
-}
module Music.Theory.Geometry.Off where

import Data.Bifunctor {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import Numeric {- base -}
import System.IO {- base -}

import Music.Theory.Geometry.Vector {- hmt-base -}

{- | Show floating point number.  k is the precision given as number of decimal places.

> show_float 3 pi == "3.141"
-}
show_float :: (RealFloat t,Show t) => Int -> t -> String
show_float k n = showFFloat (Just k) n ""

-- * Off-Cnt

-- | (n-vertex,n-face)
type Off_Cnt = (Int,Int)

-- | Parse the COUNT line.  The N-EDGES field is ignored.
off_parse_cnt :: String -> Off_Cnt
off_parse_cnt x =
  case map read (words x) of
    [v,f,_e] -> (v,f)
    _ -> error "off_parse_cnt?"

-- * Off-Face

-- | (n-vertex,[vertex-index])
type Off_Face = (Int,[Int])

-- | Off_Face is length prefixed
off_face_from_indices :: [Int] -> Off_Face
off_face_from_indices i = (length i,i)

-- | Error checking 'take' variant.
take_err :: Int -> [a] -> [a]
take_err n e = let r = take n e in if length r /= n then error "take_err?" else r

-- | Parse 'Off_Face' entry.
off_parse_face :: String -> Off_Face
off_parse_face x =
  case words x of
    n:ix -> let k = read n in (k,map read (take_err k ix))
    _ -> error "off_parse_face"

-- | Edge list of face.
off_face_edges :: Off_Face -> [V2 Int]
off_face_edges = let adj l = zip l (tail (cycle l)) in adj . snd

-- | 'v2_sort' of 'off_face_edges'
off_face_edges_undir :: Off_Face -> [V2 Int]
off_face_edges_undir = map v2_sort . off_face_edges

-- | Select faces that have indicated un-directed edge.
off_sel_edge_faces :: V2 Int -> [Off_Face] -> [Off_Face]
off_sel_edge_faces e = filter (\x -> e `elem` off_face_edges_undir x)

{-
-- | Variant of 'off_sel_edge_faces' that returns two faces at edge or errors.
off_sel_edge_faces_2 :: V2 Int -> [Off_Face] -> Maybe (Off_Face,Off_Face)
off_sel_edge_faces_2 e f =
  case off_sel_edge_faces e f of
    [p,q] -> Just (p,q)
    _ -> Nothing
-}

-- | Given face /x/ calculate the singular faces that are neighbours at each edge of /x/.
--   Allow edges to have no neighbour (ie. to be an outer edge).
off_face_neighbours :: [Off_Face] -> Off_Face -> Maybe [Off_Face]
off_face_neighbours f x =
  let e = off_face_edges_undir x
      n = map (flip off_sel_edge_faces f) e
      sel r =
        case r of
          [i] -> if i == x then Nothing else error "off_face_neighbours: NIL/NOT-EQ"
          [i,j] -> if i == x
                   then Just j
                   else if j == x
                        then Just i
                        else error "off_face_neighbours: SINGULAR/NOT-EQ"
          _ -> error (show ("off_face_neighbours: NOT-SINGULAR",x,r))
      y = mapMaybe sel n
  in if length y == fst x then Just y else Nothing

-- | Lookup index of face.
off_face_ix :: [Off_Face] -> Off_Face -> Int
off_face_ix f x = fromMaybe (error "off_face_ix?") (findIndex (== x) f)

off_dual_faces :: [Off_Face] -> [Off_Face]
off_dual_faces f =
  let to_face x = (length x,x)
  in mapMaybe (fmap (to_face . map (off_face_ix f)) . off_face_neighbours f) f

-- | True if /p/ and /q/ share at least one edge.
off_face_connected :: Off_Face -> Off_Face -> Bool
off_face_connected p q = not (null (off_face_edges_undir p `intersect` off_face_edges_undir q))

-- | Graph with /t/ as vertex label and () as edge label.
type Off_Gr t = ([(Int,t)],[((Int,Int),())])

-- | Make face connection graph.
off_face_connection_gr :: [Off_Face] -> Off_Gr Off_Face
off_face_connection_gr f =
  let v = zip [0..] f
      e = [((i,j),()) | (i,p) <- v, (j,q) <- v, i < j, off_face_connected p q]
  in (v,e)

-- * Off-Dat

-- | (vertex-seq,face-seq)
type Off_Dat t = ([t], [Off_Face])

-- | Given Off_Cnt split sequence into (V,F)
off_split_dat :: Off_Cnt -> [x] -> ([x],[x])
off_split_dat (i,j) e = (take_err i e,take_err j (drop i e))

off_parse_dat :: (String -> t) -> (Int,Int) -> [String] -> Off_Dat t
off_parse_dat f (nv,nf) = bimap (map f) (map off_parse_face) . off_split_dat (nv,nf)

-- | Rewrite a sequence of face coordinates as (vertices,[[v-indices]]).
--   Vertices are zero-indexed.
off_dat_from_face_vertex_dat :: Ord t => [[t]] -> Off_Dat t
off_dat_from_face_vertex_dat t =
  let reverse_lookup k = fmap fst . find ((== k) . snd)
      reverse_lookup_err k = fromMaybe (error "reverse_lookup") . reverse_lookup k
      p = nub (sort (concat t))
      v = zip [0..] p
      f = map (map (flip reverse_lookup_err v)) t
      length_prefix x = (length x,x)
  in (p,map length_prefix f)

-- | Lookup face by index.
off_dat_face :: Off_Dat t -> Int -> Off_Face
off_dat_face (_,f) = (!!) f

-- | Lookup face vertices.
off_dat_face_vertices :: Off_Dat t -> Off_Face -> (Int,[t])
off_dat_face_vertices (v,_) (n,i) = (n,map (v !!) i)

-- | Apply /f/ at vertices of indicated face.
off_dat_face_vertex_f :: ([t] -> u) -> Off_Dat t -> Off_Face -> u
off_dat_face_vertex_f fn dat f = fn (snd (off_dat_face_vertices dat f))

-- | Apply /f/ at vertices of all faces.
off_dat_face_set_vertex_f :: ([t] -> u) -> Off_Dat t -> [u]
off_dat_face_set_vertex_f fn dat = map (off_dat_face_vertex_f fn dat) (snd dat)

-- | Calculate dual graph, ie. 'off_face_connection_gr' with /fn/ labels.
off_dat_dual_graph :: ([t] -> u) -> Off_Dat t -> Off_Gr u
off_dat_dual_graph fn dat =
  let (v,e) = off_face_connection_gr (snd dat)
      c = off_dat_face_set_vertex_f fn dat
      f (i,_) = (i,c !! i)
  in (map f v,e)

-- | Dual of /o/, ie. vertices are centres of faces, faces are neighbours.
off_dat_dual :: ([t] -> u) -> Off_Dat t -> Off_Dat u
off_dat_dual fn dat = (off_dat_face_set_vertex_f fn dat,off_dual_faces (snd dat))

-- * Off

-- | Off file for vertex type /t/
type Off t = (Off_Cnt,Off_Dat t)

off_dat :: Off t -> Off_Dat t
off_dat (_,d) = d

off_faces :: Off t -> [Off_Face]
off_faces (_,(_,f)) = f

off_vertices :: Off t -> [t]
off_vertices (_,(v,_)) = v

off_parse :: String -> (String -> t) -> [String] -> Off t
off_parse ty f ln =
  case ln of
    hdr:cnt:dat -> let c = off_parse_cnt cnt
                   in if hdr /= ty
                      then error "off_parse: HEADER?"
                      else (c,off_parse_dat f c dat)
    _ -> error "off_parse: STRUCTURE?"

-- | Apply /f/ at vertices of /o/.
off_map_vertices :: (t -> u) -> Off t -> Off u
off_map_vertices f (cnt,(vtx,fce)) = (cnt,(map f vtx,fce))

-- | Add 'Off_Cnt' to 'Off_Dat' to make 'Off'.
off_from_off_dat :: Off_Dat t -> Off t
off_from_off_dat (v,f) = ((length v,length f),(v,f))

-- | 'off_from_off_dat' of 'off_dat_from_face_vertex_dat'
off_from_face_vertex_dat :: Ord t => [[t]] -> Off t
off_from_face_vertex_dat = off_from_off_dat . off_dat_from_face_vertex_dat

-- | Dual of /o/, ie. vertices are centres of faces, faces are neighbours.
off_dual :: ([t] -> u) -> Off t -> Off u
off_dual fn = off_from_off_dat . off_dat_dual fn . off_dat

-- | 'Off' from list of vertices and list of (non-length-prefixed) faces.
off_from_vx_fc :: ([t],[[Int]]) -> Off t
off_from_vx_fc (vx,fc) = ((length vx,length fc),(vx,map off_face_from_indices fc))

-- * Off-Txt

-- | Is line empty or a comment?
off_nil_line :: String -> Bool
off_nil_line x = null x || x !! 0 == '#'

-- | Load Off file text discarding comment and NIL lines.
off_load_txt :: FilePath -> IO [String]
off_load_txt = fmap (filter (not . off_nil_line) . lines) . readFile

-- | Given /type/ and a /vertex to list/ function, format Off file. k=precision
off_fmt :: (RealFloat u,Show u) => Int -> String -> (t -> [u]) -> Off t -> [String]
off_fmt k ty v_to_list ((nv,nf),(v,f)) =
  let v_pp = unwords . map (show_float k) . v_to_list
      pp_int = unwords . map show
      f_pp (n,i) = pp_int (n : i)
  in concat [[ty,pp_int [nv,nf,0]],map v_pp v,map f_pp f]

-- | 'writeFile' of 'off_fmt'
off_store :: (RealFloat u,Show u) => Int -> String -> (t -> [u]) -> FilePath -> Off t -> IO ()
off_store k ty v fn = writeFile fn . unlines . off_fmt k ty v

-- * Off-2 (NON-STANDARD)

-- | 'Off' of 'V2'
type Off2 t = Off (V2 t)

-- | 'v2_bounds' of 'off_vertices'.
off2_bounds :: Ord t => Off2 t -> V2 (V2 t)
off2_bounds = v2_bounds . off_vertices

off_parse_v2 :: (Read t,Fractional t) => String -> V2 t
off_parse_v2 s = case words s of {[x,y] -> (read x,read y);_ -> error "off_parse_v2"}

off2_load :: (Read t,Fractional t) => FilePath -> IO (Off2 t)
off2_load = fmap (off_parse "2Off" off_parse_v2) . off_load_txt

off2_fmt :: (RealFloat t,Show t) => Int -> Off2 t -> [String]
off2_fmt k = off_fmt k "2Off" (\(x,y) -> [x,y])

off2_store :: (RealFloat t,Show t) => Int -> FilePath -> Off2 t -> IO ()
off2_store k = off_store k "2Off" (\(x,y) -> [x,y])

-- | 'off_dual' of 'v2_centroid'.
off2_dual :: Fractional n => Off2 n -> Off2 n
off2_dual = off_dual v2_centroid

-- | 'off_dual_graph' of 'v2_centroid'
off2_dual_graph :: Fractional n => Off2 n -> Off_Gr (V2 n)
off2_dual_graph = off_dat_dual_graph v2_centroid . off_dat

-- | Lift Off2 to Off3 using a constant /z/.
off2_to_off3 :: t -> Off2 t -> Off3 t
off2_to_off3 z (cnt,(vtx,fc)) = (cnt,(map (\(x,y) -> (x,y,z)) vtx,fc))

-- * Off-3

-- | 'Off' of 'V3'
type Off3 t = Off (V3 t)

off3_bounds :: Ord t => Off3 t -> V2 (V3 t)
off3_bounds = v3_bounds . off_vertices

off_parse_v3 :: (Read t,Fractional t) => String -> V3 t
off_parse_v3 s = case words s of {[x,y,z] -> (read x,read y,read z);_ -> error "off_parse_v3"}

off3_load :: (Read t,Fractional t) => FilePath -> IO (Off3 t)
off3_load = fmap (off_parse "Off" off_parse_v3) . off_load_txt

off3_fmt :: (RealFloat t,Show t) => Int -> Off3 t -> [String]
off3_fmt k = off_fmt k "Off" (\(x,y,z) -> [x,y,z])

-- | 'off_store' of three-tuple
off3_store :: (RealFloat t,Show t) => Int -> FilePath -> Off3 t -> IO ()
off3_store k = off_store k "Off" (\(x,y,z) -> [x,y,z])

-- | 'off3_store' . 'off_from_vx_fc'
off3_store_vx_fc :: (RealFloat t,Show t) => Int -> FilePath -> ([V3 t],[[Int]]) -> IO ()
off3_store_vx_fc k fn = off3_store k fn . off_from_vx_fc

-- | 'off_dual' of 'v3_centroid'.
off3_dual :: Fractional n => Off3 n -> Off3 n
off3_dual = off_dual v3_centroid

-- | 'off_dual_graph' of 'v3_centroid'
off3_dual_graph :: Fractional n => Off3 n -> Off_Gr (V3 n)
off3_dual_graph = off_dat_dual_graph v3_centroid . snd

-- * Off-4

type Off4 t = Off (V4 t)

off4_bounds :: Ord t => Off4 t -> V2 (V4 t)
off4_bounds = v4_bounds . off_vertices

off_parse_v4 :: (Read t,Fractional t) => String -> V4 t
off_parse_v4 txt =
  case words txt of
    [x,y,z,w] -> (read x,read y,read z,read w)
    _ -> error "off_parse_v4?"

off4_load :: (Read t,Fractional t) => FilePath -> IO (Off4 t)
off4_load = fmap (off_parse "4Off" off_parse_v4) . off_load_txt

off4_fmt :: (RealFloat t,Show t) => Int -> Off4 t -> [String]
off4_fmt k = off_fmt k "4Off" (\(x,y,z,w) -> [x,y,z,w])

off4_store :: (RealFloat t,Show t) => Int -> FilePath -> Off4 t -> IO ()
off4_store k = off_store k "4Off" (\(x,y,z,w) -> [x,y,z,w])

-- * Off

-- | Read the TYPE entry for an Off file, ie. Off or 4Off.
off_load_type :: FilePath -> IO String
off_load_type fn = withFile fn ReadMode hGetLine

-- | Load either Off3 or Off4 data file.
off_load :: (Read t,Fractional t) => FilePath -> IO (Either (Off3 t) (Off4 t))
off_load fn = do
  ty <- off_load_type fn
  case ty of
    "Off" -> fmap Left (off3_load fn)
    "4Off" -> fmap Right (off4_load fn)
    _ -> error "off_load: TYPE?"

off_load_either :: (Read t,Fractional t) => (V3 t -> u,V4 t -> u) -> FilePath -> IO (Off u)
off_load_either (v3_f,v4_f) = fmap (either (off_map_vertices v3_f) (off_map_vertices v4_f)) . off_load

-- * Analysis

off_edge_grp :: [Off_Face] -> [[(Int,Int)]]
off_edge_grp =
  let f (_,x) = zip (last x : x) x
      g (i,j) = (min i j,max i j)
  in group . sort . map g . concatMap f

off_edge_set :: [Off_Face] -> [(Int,Int)]
off_edge_set = map head . off_edge_grp

-- | Graph of Off data.  Vertices are labeled with co-ordinates, edges are un-labeled.
off_graph :: Off t -> Off_Gr t
off_graph (_,(v,f)) = (zip [0..] v,zip (off_edge_set f) (repeat ()))

-- | Simple histogram function.
off_hist :: Ord t => [t] -> [(t,Int)]
off_hist = let f x = (head x,length x) in map f . group . sort

-- | Vertex histogram (VERTEX,N-ENTRIES)
off_vertex_hist :: [Off_Face] -> [(Int,Int)]
off_vertex_hist = off_hist . concatMap snd

-- | Vertices in only one face.
off_v_uniq_in_f :: Off t -> [Int]
off_v_uniq_in_f  = map fst . filter ((==) 1 . snd) . off_vertex_hist . off_faces

-- | Vertices not in any face.
off_v_not_in_f :: Off t -> [Int]
off_v_not_in_f ((nv,_nf),(_v,f)) =
  let e = off_edge_set f
      u = nub (sort (concatMap (\(i,j) -> [i,j]) e))
  in [0 .. nv - 1] \\ u

-- | Collect duplicate faces (with occurence count)
off_duplicate_faces :: Off t -> [(Int,Off_Face)]
off_duplicate_faces (_,(_,f)) =
  let col x = (length x,head x)
  in filter ((> 1) . fst) (map col (group (sort f)))

-- | Degree of each face at 'Off'
off_face_degrees :: Off t -> [Int]
off_face_degrees (_,(_,fc)) = map fst fc

-- | Set of 'off_face_degrees'
off_face_degrees_set :: Off t -> [Int]
off_face_degrees_set = nub . sort . off_face_degrees

-- | Select all 'Off_Face' of indicated degree.
off_faces_of_degree :: Off t -> Int -> [Off_Face]
off_faces_of_degree (_,(_,fc)) k = filter (\(n,_) -> n == k) fc

-- | 'findIndex' of 'Off_Face at 'Off'
off_face_index :: Off t -> Off_Face -> Maybe Int
off_face_index (_,(_,fc)) x = findIndex (== x) fc

-- | 'error' variant
off_face_index_err :: Off t -> Off_Face -> Int
off_face_index_err o = fromMaybe (error "off_face_index?") . off_face_index o

-- | Find index of first face at 'Off' that has maximal degree.
off_first_max_degree_face :: Off t -> Int
off_first_max_degree_face o =
  let dgr = off_face_degrees_set o
  in case off_faces_of_degree o (maximum dgr) of
       fc:_ -> off_face_index_err o fc
       _ -> error "off_first_max_degree_face"

-- * Stat

-- | Statistics.
off_stat :: Off t -> [String]
off_stat o =
  let ((nv,nf),(_v,f)) = o
      jn (i,j) = if null j then Nothing else Just (concat [i,": ",j])
      hpp (i,j) = concat [show i,"⋅",show j]
  in mapMaybe
     jn
     [("N-VERTICES",show nv)
     ,("N-FACES",show nf)
     ,("FACE-DEGREES",unwords (map hpp (off_hist (map fst f))))
     ,("N-EDGES",show (sum (map fst f)))
     ,("N-EDGES-UNIQ",show (length (off_edge_set f)))
     ,("V-NOT-IN-F",unwords (map show (off_v_not_in_f o)))
     ,("V-UNIQ-IN-F",unwords (map show (off_v_uniq_in_f o)))]

off_stat_wr :: Off t -> IO ()
off_stat_wr = putStr . unlines . off_stat

-- * Obj

-- | Translate Off3 to OBJ format (indices are ONE-INDEXED)
off3_to_obj :: (Show t) => Off3 t -> [String]
off3_to_obj (_,(v,f)) =
  let v_pp (x,y,z) = unwords ("v" : map show [x,y,z])
      f_pp (_,i) = unwords ("f" : map (show . (+ 1)) i)
  in map v_pp v ++ map f_pp f

-- * Float 64 (Double-precision)

-- | Type specialised
off2_load_f64 :: FilePath -> IO (Off2 Double)
off2_load_f64 = off2_load

-- | Type specialised
off3_load_f64 :: FilePath -> IO (Off3 Double)
off3_load_f64 = off3_load

-- | Type specialised
off4_load_f64 :: FilePath -> IO (Off4 Double)
off4_load_f64 = off4_load

-- | Type specialised
off_load_f64 :: FilePath -> IO (Either (Off3 Double) (Off4 Double))
off_load_f64 = off_load

-- * Colour Face Set

-- | Rewrite a set of faces as (vertices,[[v-indices]]).
--   Indices are zero-indexed.
off_clr_face_set_dat :: Ord n => [([(n,n,n)],(i,i,i))] -> ([(Int,(n,n,n))],[([Int],(i,i,i))])
off_clr_face_set_dat t =
  let reverse_lookup k = fmap fst . find ((== k) . snd)
      reverse_lookup_err k = fromMaybe (error "reverse_lookup") . reverse_lookup k
      p = nub (sort (concat (map fst t)))
      c = map snd t
      v = zip [0..] p
      f = map (map (flip reverse_lookup_err v)) (map fst t)
  in (v,zip f c)

off_header :: (Int,Int,Int) -> [String]
off_header (v,f,e) = ["Off",unwords (map show [v,f,e])]

-- | Format a set of coloured faces as an Off file.
--   (Ccw triples of (x,y,z) coordinates, (r,g,b) colour)
--   Off files are one-indexed.
off_clr_face_set_fmt :: (RealFloat n,Show n,Ord n,Show i) => Int -> [([V3 n],V3 i)] -> [String]
off_clr_face_set_fmt k t =
  let v_f (_,(x,y,z)) = unwords (map (show_float k) [x,y,z])
      f_f (ix,(r,g,b)) = unwords (map show (length ix : ix) ++ map show [r,g,b])
      (v,f) = off_clr_face_set_dat t
  in concat [off_header (length v,length f,0), map v_f v, map f_f f]

-- | 'writeFile' of 'off_clr_face_set_fmt'
off_clr_face_set_store :: (RealFloat n,Show n,Ord n,Show i) => Int -> FilePath -> [([V3 n],V3 i)] -> IO ()
off_clr_face_set_store k fn = writeFile fn . unlines . off_clr_face_set_fmt k

-- * Cli

-- | 'writeFile' of 'off3_to_obj' of 'off3_load_f64'
cli_off3_to_obj :: FilePath -> FilePath -> IO ()
cli_off3_to_obj off_fn obj_fn = do
  o <- off3_load_f64 off_fn
  writeFile obj_fn (unlines (off3_to_obj o))
