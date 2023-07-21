-- | Keys are given in the header, empty fields are omitted from records.
module Music.Theory.Db.Csv where

import Data.Maybe {- base -}

import qualified Text.CSV.Lazy.String as Csv {- lazy-csv -}

import qualified Music.Theory.Io as Io {- hmt-base -}

import qualified Music.Theory.Db.Common as Common {- hmt -}

-- | Load 'TextDb' from 'FilePath'.
db_load_utf8 :: FilePath -> IO Common.TextDb
db_load_utf8 fn = do
  s <- Io.read_file_utf8 fn
  let p = Csv.fromCSVTable (Csv.csvTable (Csv.parseCSV s))
      (h,d) = (head p,tail p)
      f k v = if null v then Nothing else Just (k,v)
  return (map (catMaybes . zipWith f h) d)

db_store_utf8 :: FilePath -> Common.TextDb -> IO ()
db_store_utf8 fn db = do
  let (hdr,tbl) = Common.db_to_table (fromMaybe "") db
      (_,tbl') = Csv.toCSVTable (hdr : tbl)
      str = Csv.ppCSVTable tbl'
  Io.write_file_utf8 fn str
