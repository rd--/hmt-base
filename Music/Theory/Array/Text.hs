-- | Regular array data as plain text tables.
module Music.Theory.Array.Text where

import Data.List {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Array as Array {- hmt-base -}
import qualified Music.Theory.Function as Function {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.String as String {- hmt-base -}

-- | Tabular text.
type Text_Table = [[String]]

{- | Split table at indicated places.

>>> let tbl = [["1","2","3","4"],["A","B","E","F"],["C","D","G","H"]]
>>> table_split [2,2] tbl
[[["1","2"],["A","B"],["C","D"]],[["3","4"],["E","F"],["G","H"]]]
-}
table_split :: [Int] -> Text_Table -> [Text_Table]
table_split pl dat = transpose (map (Split.splitPlaces pl) dat)

{- | Join tables left to right.

>>> table_concat [[["1","2"],["A","B"],["C","D"]],[["3","4"],["E","F"],["G","H"]]]
[["1","2","3","4"],["A","B","E","F"],["C","D","G","H"]]
-}
table_concat :: [Text_Table] -> Text_Table
table_concat sq = map concat (transpose sq)

{- | Add a row number column at the front of the table.

>>> table_number_rows 0 [["1","2","3","4"],["A","B","E","F"],["C","D","G","H"]]
[["0","1","2","3","4"],["1","A","B","E","F"],["2","C","D","G","H"]]
-}
table_number_rows :: Int -> Text_Table -> Text_Table
table_number_rows k = zipWith (\i r -> show i : r) [k ..]

{- | (Header,Pad-Left,Equal-Width,Column-Separator,Table-Delimiters).

Options are:
 has header
 pad text with space to left instead of right,
 make all columns equal width,
 column separator string,
 print table delimiters
-}
type Text_Table_Opt = (Bool, Bool, Bool, String, Bool)

-- | Options for @plain@ layout.
table_opt_plain :: Text_Table_Opt
table_opt_plain = (False, True, False, " ", False)

-- | Options for @simple@ layout.
table_opt_simple :: Text_Table_Opt
table_opt_simple = (True, True, False, " ", True)

-- | Options for @pipe@ layout.
table_opt_pipe :: Text_Table_Opt
table_opt_pipe = (True, True, False, " | ", False)

{- | Pretty-print table.  Table is in row order.

>>> let tbl = [["1","2","3","4"],["a","bc","def"],["ghij","klm","no","p"]]
>>> table_pp (True,True,True," ",True) tbl
["   1    2    3    4","---- ---- ---- ----","   a   bc  def","ghij  klm   no    p","---- ---- ---- ----"]

>>> table_pp (False,False,True," ",False) tbl
["1    2    3    4","a    bc   def","ghij klm  no   p"]
-}
table_pp :: Text_Table_Opt -> Text_Table -> [String]
table_pp (has_hdr, pad_left, eq_width, col_sep, print_eot) dat =
  let c = transpose (Array.tbl_make_regular_nil "" dat)
      nc = length c
      n =
        let k = map (maximum . map length) c
        in if eq_width then replicate nc (maximum k) else k
      ext k s = if pad_left then List.pad_left ' ' k s else List.pad_right ' ' k s
      jn = concat . intersperse col_sep
      m = jn (map (`replicate` '-') n)
      w = map jn (transpose (zipWith (map . ext) n c))
      d = map String.delete_trailing_whitespace w
      pr x = if print_eot then List.bracket (m, m) x else x
  in case d of
      [] -> error "table_pp"
      d0 : dr -> if has_hdr then d0 : pr dr else pr d

{- | Variant relying on 'Show' instances.

>>> table_pp_show table_opt_simple [[1..4],[5..8],[9..12]]
["1  2  3  4","- -- -- --","5  6  7  8","9 10 11 12","- -- -- --"]
-}
table_pp_show :: Show t => Text_Table_Opt -> Array.Table t -> [String]
table_pp_show opt = table_pp opt . map (map show)

{- | Variant in column order (ie. 'transpose').

>>> table_pp_column_order table_opt_simple [["a","bc","def"],["ghij","klm","no"]]
["  a ghij","--- ----"," bc  klm","def   no","--- ----"]
-}
table_pp_column_order :: Text_Table_Opt -> Text_Table -> [String]
table_pp_column_order opt = table_pp opt . transpose

{- | Matrix form, ie. header in both first row and first column, in
each case displaced by one location which is empty.

>>> let h = (map return "abc",map return "efgh")
>>> let t = table_matrix h (map (map show) [[1,2,3,4],[2,3,4,1],[3,4,1,2]])
>>> table_pp table_opt_simple t
["  e f g h","- - - - -","a 1 2 3 4","b 2 3 4 1","c 3 4 1 2","- - - - -"]
-}
table_matrix :: ([String], [String]) -> Text_Table -> Text_Table
table_matrix (r, c) t = table_concat [[""] : map return r, c : t]

{- | Variant that takes a 'show' function and a /header decoration/ function.

>>> table_matrix_opt show id ([1,2,3],[4,5,6]) [[7,8,9],[10,11,12],[13,14,15]]
[["","4","5","6"],["1","7","8","9"],["2","10","11","12"],["3","13","14","15"]]
-}
table_matrix_opt :: (a -> String) -> (String -> String) -> ([a], [a]) -> Array.Table a -> Text_Table
table_matrix_opt show_f hd_f nm t =
  let nm' = Function.bimap1 (map (hd_f . show_f)) nm
      t' = map (map show_f) t
  in table_matrix nm' t'

{-
-- | Two-tuple 'show' variant.
table_table_p2 :: (Show a,Show b) => Text_Table_Opt -> Maybe [String] -> ([a],[b]) -> [String]
table_table_p2 opt hdr (p,q) = table_table' opt hdr [map show p,map show q]

-- | Three-tuple 'show' variant.
table_table_p3 :: (Show a,Show b,Show c) => Text_Table_Opt -> Maybe [String] -> ([a],[b],[c]) -> [String]
table_table_p3 opt hdr (p,q,r) = table_table' opt hdr [map show p,map show q,map show r]

-}
