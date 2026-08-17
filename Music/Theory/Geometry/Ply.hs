{- | Ply functions.

This module is used instead of 'Music.Theory.Geometry.Obj' when faces are coloured.

There is no reader.

Greg Turk "The Ply Polygon File Format" (1994)

See "Ply_Files.txt" in <https://www.cc.gatech.edu/projects/large_models/files/ply.tar.gz>

See also: <https://www.loc.gov/preservation/digital/formats/fdd/fdd000501.shtml>
-}
module Music.Theory.Geometry.Ply where

import qualified Music.Theory.Geometry.Vector as Vector {- hmt-base -}
import qualified Music.Theory.Graph.Type as Graph {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Show as Show {- hmt-base -}

{- | Ascii Ply-1.0 header for object of (n-vertices,n-faces,n-edges).
     Faces and edges are (r,g,b) coloured.

>>> putStr $ unlines $ ply_header (8,6,0) False
ply
format ascii 1.0
element vertex 8
property float x
property float y
property float z
element face 6
property list uchar int vertex_index
property uchar red
property uchar green
property uchar blue
end_header
-}
ply_header :: Vector.V3 Int -> Bool -> [String]
ply_header (n_v, n_f, n_e) v_coloured =
  let colour_properties =
        [ "property uchar red"
        , "property uchar green"
        , "property uchar blue"
        ]
  in concat
      [ [ "ply"
        , "format ascii 1.0"
        , "element vertex " ++ show n_v
        , "property float x"
        , "property float y"
        , "property float z"
        ]
          ++ if v_coloured then colour_properties else []
      , if n_f > 0
          then
            [ "element face " ++ show n_f
            , "property list uchar int vertex_index"
            ]
              ++ colour_properties
          else []
      , if n_e > 0
          then
            [ "element edge " ++ show n_e
            , "property int vertex1"
            , "property int vertex2"
            ]
              ++ colour_properties
          else []
      , ["end_header"]
      ]

{- | Requires (but does not check) that graph vertices be indexed [0 .. #v - 1]
     Edges are coloured as U8 (red,green,blue) triples.
     It is an error (not checked) for there to be no edges.
     Ply files are zero-indexed.
-}
v3_graph_to_ply_clr :: Int -> Graph.Lbl (Vector.V3 Double) (Vector.V3 Int) -> [String]
v3_graph_to_ply_clr k (v, e) =
  let v_pp (_, (x, y, z)) = unwords (map (Show.double_pp k) [x, y, z])
      e_pp ((i, j), (r, g, b)) = unwords (map show [i, j, r, g, b])
  in concat
      [ ply_header (length v, 0, length e) False
      , map v_pp v
      , map e_pp e
      ]

-- | n-colour point set to Ply text. k=precision.  pt_set=[([(x,y,z)],(r,g,b))]
v3_pt_set_to_ply :: Int -> [([Vector.V3 Double], Vector.V3 Int)] -> [String]
v3_pt_set_to_ply k pt_set =
  let n_v = sum (map (length . fst) pt_set)
      h = ply_header (n_v, 0, 0) True
      v_pp (r, g, b) (x, y, z) =
        unwords
          ( concat
              [ map (Show.double_pp k) [x, y, z]
              , map show [r, g, b]
              ]
          )
  in h ++ concatMap (\(v, c) -> map (v_pp c) v) pt_set

-- * Faces

-- | Ply face set (vertices=[(x,y,z)],colour=(r,g,b))
type Ply_Face_Set n i = ([Vector.V3 n], Vector.V3 i)

-- | Ply data (vertices=[(ix,(x,y,z))],faces=[([ix],(r,g,b))])
type Ply_Dat n i = ([(Int, Vector.V3 n)], [([Int], Vector.V3 i)])

{- | Rewrite a list of Ply_Face_Set as Ply_Dat
  Indices are zero-indexed.
-}
ply_face_set_dat :: Ord n => [Ply_Face_Set n i] -> Ply_Dat n i
ply_face_set_dat t =
  let p = List.nub_sort (concatMap fst t)
      c = map snd t
      v = zip [0 ..] p
      f = map (map (`List.reverse_lookup_err` v) . fst) t
  in (v, zip f c)

{- | Format a set of coloured faces as an Ply file.
   (CCW triples of (x,y,z) coordinates, (r,g,b) colour)
  Ply files are one-indexed.
-}
ply_face_set_fmt :: (Show n, Ord n, Show i) => [Ply_Face_Set n i] -> [String]
ply_face_set_fmt t =
  let v_f (_, (x, y, z)) = unwords [show x, show y, show z]
      f_f (ix, (r, g, b)) = unwords (map show (length ix : ix) ++ map show [r, g, b])
      (v, f) = ply_face_set_dat t
  in concat [ply_header (length v, length f, 0) False, map v_f v, map f_f f]

-- | 'writeFile' of 'ply_face_set_fmt'
ply_face_set_store :: (Show n, Ord n, Show i) => FilePath -> [Ply_Face_Set n i] -> IO ()
ply_face_set_store fn = writeFile fn . unlines . ply_face_set_fmt
