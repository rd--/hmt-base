-- | <http://www.tcs.hut.fi/Software/bliss/fileformat.shtml>
module Music.Theory.Graph.Bliss where

import qualified Music.Theory.Graph.Type as Graph {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}

-- | Problem is (n-vertices,n-edges)
bliss_parse_problem :: String -> (Int,Int)
bliss_parse_problem txt =
  case words txt of
    ["p","edge",n,e] -> (read n,read e)
    _ -> error "bliss_parse_problem"

-- | Vertex colour is (vertex,colour)
bliss_parse_vertex_colour :: String -> (Int,Int)
bliss_parse_vertex_colour txt =
  case words txt of
    ["n",v,e] -> (read v,read e)
    _ -> error "bliss_parse_vertex_color"

-- | Edge is (vertex,vertex)
bliss_parse_edge :: String -> (Int,Int)
bliss_parse_edge txt =
  case words txt of
    ["e",v1,v2] -> (read v1,read v2)
    _ -> error "bliss_parse_edge"

-- | (problem,vertex-colours,edges)
--   Bliss data is one-indexed.
type Bliss = ((Int,Int), [(Int,Int)], [(Int,Int)])

-- | Parse 'Bliss'
bliss_parse :: String -> Bliss
bliss_parse txt =
  let c0_is x = (== x) . List.head_err
      ln = dropWhile (c0_is 'c') (lines txt) -- c = comment
      ([p],r1) = span (c0_is 'p') ln -- p = problem
      (n,r2) = span (c0_is 'n') r1 -- n = vertex colour
      (e,_) = span (c0_is 'e') r2 -- e = edge
  in (bliss_parse_problem p,map bliss_parse_vertex_colour n,map bliss_parse_edge e)

-- | 'bliss_parse' of 'readFile'
bliss_load :: FilePath -> IO Bliss
bliss_load = fmap bliss_parse . readFile

-- | 'Bliss' (one-indexed) to 'Graph.G' (zero-indexed)
bliss_to_g :: Bliss -> Graph.G
bliss_to_g ((k,_),_,e) = ([0 .. k - 1],map (\(i,j) -> (i - 1,j - 1)) e)
