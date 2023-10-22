-- | Regular matrix array data, Csv, column & row indexing.
module Music.Theory.Array.Csv where

import Data.List {- base -}

import qualified Data.Array {- array -}
import qualified Safe {- safe -}
import qualified Text.CSV.Lazy.String as Csv {- lazy-csv -}

import qualified Music.Theory.Array as Array {- hmt-base -}
import qualified Music.Theory.Array.Cell_Ref as Cell_Ref {- hmt-base -}
import qualified Music.Theory.Io as Io {- hmt-base -}
import qualified Music.Theory.List as List {- hmt-base -}
import qualified Music.Theory.Tuple as Tuple {- hmt-base -}

-- * Field / Quote

-- | Quoting is required is the string has a double-quote, comma newline or carriage-return.
csv_requires_quote :: String -> Bool
csv_requires_quote = any (`elem` "\",\n\r")

-- | Quoting places double-quotes at the start and end and escapes double-quotes.
csv_quote :: String -> String
csv_quote fld =
  let esc s =
        case s of
          [] -> []
          '"':s' -> '"' : '"' : esc s'
          c:s' -> c : esc s'
  in '"' : esc fld ++ "\""

-- | Quote field if required.
csv_quote_if_req :: String -> String
csv_quote_if_req fld = if csv_requires_quote fld then csv_quote fld else fld

-- * Table

-- | When reading a CSV file is the first row a header?
type Csv_Has_Header = Bool

-- | Alias for 'Char', allow characters other than @,@ as delimiter.
type Csv_Delimiter = Char

-- | Alias for 'Bool', allow linebreaks in fields.
type Csv_Allow_Linebreaks = Bool

-- | When writing a CSV file should the delimiters be aligned,
-- ie. should columns be padded with spaces, and if so at which side
-- of the data?
data Csv_Align_Columns = Csv_No_Align | Csv_Align_Left | Csv_Align_Right

-- | CSV options.
type Csv_Opt = (Csv_Has_Header,Csv_Delimiter,Csv_Allow_Linebreaks,Csv_Align_Columns)

-- | Default CSV options, no header, comma delimiter, no linebreaks, no alignment.
def_csv_opt :: Csv_Opt
def_csv_opt = (False,',',False,Csv_No_Align)

-- | CSV table, ie. a 'Table' with 'Maybe' a header.
type Csv_Table a = (Maybe [String],Array.Table a)

-- | Read 'Csv_Table' from @CSV@ file.
csv_table_read :: Csv_Opt -> (String -> a) -> FilePath -> IO (Csv_Table a)
csv_table_read (hdr,delim,brk,_) f fn = do
  s <- Io.read_file_utf8 fn
  let t = Csv.csvTable (Csv.parseDSV brk delim s)
      p = Csv.fromCSVTable t
      (h,d) = if hdr then (Just (List.head_err p),List.tail_err p) else (Nothing,p)
  return (h,map (map f) d)

-- | Read 'Array.Table' only with 'def_csv_opt'.
csv_table_read_def :: (String -> a) -> FilePath -> IO (Array.Table a)
csv_table_read_def f = fmap snd . csv_table_read def_csv_opt f

-- | Read plain CSV 'Array.Table'.
csv_table_read_plain :: FilePath -> IO (Array.Table String)
csv_table_read_plain = csv_table_read_def id

-- | Read and process @CSV@ 'Csv_Table'.
csv_table_with :: Csv_Opt -> (String -> a) -> FilePath -> (Csv_Table a -> b) -> IO b
csv_table_with opt f fn g = fmap g (csv_table_read opt f fn)

{- | Align table according to 'Csv_Align_Columns'.

>>> csv_table_align Csv_No_Align [["a","row","and"],["then","another","one"]]
[["a","row","and"],["then","another","one"]]
-}
csv_table_align :: Csv_Align_Columns -> Array.Table String -> Array.Table String
csv_table_align align tbl =
    let c = transpose tbl
        n = map (maximum . map length) c
        ext k s = let pd = replicate (k - length s) ' '
                  in case align of
                       Csv_No_Align -> s
                       Csv_Align_Left -> pd ++ s
                       Csv_Align_Right -> s ++ pd
    in transpose (zipWith (map . ext) n c)

-- | Pretty-print 'Csv_Table'.
csv_table_pp :: (a -> String) -> Csv_Opt -> Csv_Table a -> String
csv_table_pp f (_,delim,brk,align) (hdr,tbl) =
  let tbl' = csv_table_align align (List.mcons hdr (map (map f) tbl))
      (_,t) = Csv.toCSVTable tbl'
  in Csv.ppDSVTable brk delim t

-- | 'Io.write_file_utf8' of 'csv_table_pp'.
csv_table_write :: (a -> String) -> Csv_Opt -> FilePath -> Csv_Table a -> IO ()
csv_table_write f opt fn csv = Io.write_file_utf8 fn (csv_table_pp f opt csv)

-- | Write 'Table' only (no header) with 'def_csv_opt'.
csv_table_write_def :: (a -> String) -> FilePath -> Array.Table a -> IO ()
csv_table_write_def f fn tbl = csv_table_write f def_csv_opt fn (Nothing,tbl)

-- | Write plain CSV 'Table'.
csv_table_write_plain :: FilePath -> Array.Table String -> IO ()
csv_table_write_plain = csv_table_write_def id

-- | @0@-indexed (row,column) cell lookup.
table_lookup :: Array.Table a -> (Int,Int) -> a
table_lookup t (r,c) = let ix = Safe.atNote "table_lookup" in (t `ix` r) `ix` c

-- | Row data.
table_row :: Array.Table a -> Cell_Ref.Row_Ref -> [a]
table_row t r = Safe.atNote "table_row" t (Cell_Ref.row_index r)

-- | Column data.
table_column :: Array.Table a -> Cell_Ref.Column_Ref -> [a]
table_column t c = Safe.atNote "table_column" (transpose t) (Cell_Ref.column_index c)

-- | Lookup value across columns.
table_column_lookup :: Eq a => Array.Table a -> (Cell_Ref.Column_Ref,Cell_Ref.Column_Ref) -> a -> Maybe a
table_column_lookup t (c1,c2) e =
    let a = zip (table_column t c1) (table_column t c2)
    in lookup e a

-- | Table cell lookup.
table_cell :: Array.Table a -> Cell_Ref.Cell_Ref -> a
table_cell t (c,r) =
    let (r',c') = (Cell_Ref.row_index r,Cell_Ref.column_index c)
    in table_lookup t (r',c')

-- | @0@-indexed (row,column) cell lookup over column range.
table_lookup_row_segment :: Array.Table a -> (Int,(Int,Int)) -> [a]
table_lookup_row_segment t (r,(c0,c1)) =
    let r' = Safe.atNote "table_lookup_row_segment" t r
    in take (c1 - c0 + 1) (drop c0 r')

-- | Range of cells from row.
table_row_segment :: Array.Table a -> (Cell_Ref.Row_Ref,Cell_Ref.Column_Range) -> [a]
table_row_segment t (r,c) =
    let (r',c') = (Cell_Ref.row_index r,Cell_Ref.column_indices c)
    in table_lookup_row_segment t (r',c')

-- * Array

{- | Translate 'Table' to 'Array'.
It is assumed that the 'Table' is regular, ie. all rows have an equal number of columns.

>>> let a = table_to_array [[0,1,3],[2,4,5]]
>>> (Data.Array.bounds a,Data.Array.indices a,Data.Array.elems a)
(((A,1),(C,2)),[(A,1),(A,2),(B,1),(B,2),(C,1),(C,2)],[0,2,1,4,3,5])
-}
table_to_array :: Array.Table a -> Data.Array.Array Cell_Ref.Cell_Ref a
table_to_array t =
    let nr = length t
        nc = length (Safe.atNote "table_to_array" t 0)
        bnd = (Cell_Ref.cell_ref_minima,(toEnum (nc - 1),nr))
        asc = zip (Cell_Ref.cell_range_row_order bnd) (concat t)
    in Data.Array.array bnd asc

-- | 'table_to_array' of 'csv_table_read'.
csv_array_read :: Csv_Opt -> (String -> a) -> FilePath -> IO (Data.Array.Array Cell_Ref.Cell_Ref a)
csv_array_read opt f fn = fmap (table_to_array . snd) (csv_table_read opt f fn)

-- * Irregular

csv_field_str :: Csv.CSVField -> String
csv_field_str f =
    case f of
      Csv.CSVField _ _ _ _ s _ -> s
      Csv.CSVFieldError _ _ _ _ _ -> error "csv_field_str"

csv_error_recover :: Csv.CSVError -> Csv.CSVRow
csv_error_recover e =
    case e of
      Csv.IncorrectRow _ _ _ f -> f
      Csv.BlankLine _ _ _ _ -> []
      _ -> error "csv_error_recover: not recoverable"

csv_row_recover :: Either [Csv.CSVError] Csv.CSVRow -> Csv.CSVRow
csv_row_recover r =
    case r of
      Left [e] -> csv_error_recover e
      Left _ -> error "csv_row_recover: multiple errors"
      Right r' -> r'

-- | Read irregular @CSV@ file, ie. rows may have any number of columns, including no columns.
csv_load_irregular :: (String -> a) -> FilePath -> IO [[a]]
csv_load_irregular f fn = do
  s <- Io.read_file_utf8 fn
  return (map (map (f . csv_field_str) . csv_row_recover) (Csv.parseCSV s))

csv_write_irregular :: (a -> String) -> Csv_Opt -> FilePath -> Csv_Table a -> IO ()
csv_write_irregular f opt fn (hdr,tbl) =
  let tbl' = Array.tbl_make_regular_nil "" (map (map f) tbl)
  in Io.write_file_utf8 fn (csv_table_pp id opt (hdr,tbl'))

csv_write_irregular_def :: (a -> String) -> FilePath -> Array.Table a -> IO ()
csv_write_irregular_def f fn tbl = csv_write_irregular f def_csv_opt fn (Nothing,tbl)

-- * Tuples

type P2_Parser t1 t2 = (String -> t1,String -> t2)

csv_table_read_p2 :: P2_Parser t1 t2 -> Csv_Opt -> FilePath -> IO (Maybe (String,String),[(t1,t2)])
csv_table_read_p2 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (fmap Tuple.t2_from_list hdr,map (Tuple.p2_from_list f) dat)

type P5_Parser t1 t2 t3 t4 t5 = (String -> t1,String -> t2,String -> t3,String -> t4,String -> t5)
type P5_Writer t1 t2 t3 t4 t5 = (t1 -> String,t2 -> String,t3 -> String,t4 -> String,t5 -> String)

csv_table_read_p5 :: P5_Parser t1 t2 t3 t4 t5 -> Csv_Opt -> FilePath -> IO (Maybe [String],[(t1,t2,t3,t4,t5)])
csv_table_read_p5 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (hdr,map (Tuple.p5_from_list f) dat)

csv_table_write_p5 :: P5_Writer t1 t2 t3 t4 t5 -> Csv_Opt -> FilePath -> (Maybe [String],[(t1,t2,t3,t4,t5)]) -> IO ()
csv_table_write_p5 f opt fn (hdr,dat) = csv_table_write id opt fn (hdr,map (Tuple.p5_to_list f) dat)

csv_table_read_t9 :: (String -> t) -> Csv_Opt -> FilePath -> IO (Maybe [String],[Tuple.T9 t])
csv_table_read_t9 f opt fn = do
  (hdr,dat) <- csv_table_read opt id fn
  return (hdr,map (Tuple.t9_from_list . map f) dat)

