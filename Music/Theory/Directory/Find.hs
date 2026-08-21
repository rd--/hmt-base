-- | Directory functions using 'find' system utility.
module Music.Theory.Directory.Find where

import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}

import qualified System.Process {- process -}

{- | Find files having indicated filename.
This runs the system utility /find/, so is Unix only.

>>> dir_find "DX7-ROM1A.syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
["/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"]
-}
dir_find :: FilePath -> FilePath -> IO [FilePath]
dir_find fn dir = fmap lines (System.Process.readProcess "find" [dir, "-name", fn] "")

{- | Require that exactly one file is located, else error.

>>> dir_find_1 "DX7-ROM1A.syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
"/home/rohan/sw/hsc3-data/data/yamaha/dx7/rom/DX7-ROM1A.syx"
-}
dir_find_1 :: FilePath -> FilePath -> IO FilePath
dir_find_1 fn dir = do
  r <- dir_find fn dir
  case r of
    [x] -> return x
    _ -> error "dir_find_1?"

{- | Recursively find files having case-insensitive filename extension.
This runs the system utility /find/, so is Unix only.

>>> r <- dir_find_ext ".syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
>>> length r
220
-}
dir_find_ext :: String -> FilePath -> IO [FilePath]
dir_find_ext ext dir = fmap lines (System.Process.readProcess "find" [dir, "-iname", '*' : ext] "")

{- | Post-process 'dir_find_ext' to delete starting directory.

>>> r <- dir_find_ext_rel ".syx" "/home/rohan/sw/hsc3-data/data/yamaha/"
>>> length r
220
-}
dir_find_ext_rel :: String -> FilePath -> IO [FilePath]
dir_find_ext_rel ext dir =
  let f = Data.Maybe.fromMaybe (error "dir_find_ext_rel?")
          . Data.List.stripPrefix dir
  in fmap (map f) (dir_find_ext ext dir)

{- | Scan each directory on path recursively for file.
Stop once a file is located.
Runs 'dir_find' so is Unix only.

>>> path_scan_recursively ["/home/rohan/sw/hmt-base"] "Directory.hs"
Just "/home/rohan/sw/hmt-base/Music/Theory/Directory.hs"
-}
path_scan_recursively :: [FilePath] -> FilePath -> IO (Maybe FilePath)
path_scan_recursively p fn =
  case p of
    [] -> return Nothing
    dir : p' -> do
      r <- dir_find fn dir
      case r of
        [] -> path_scan_recursively p' fn
        x : _ -> return (Just x)

{- | Search each directory on path recursively for file.
Runs 'dir_find' so is Unix only.

>>> r <- path_search_recursively ["/home/rohan/sw"] "README.md"
>>> length r
172
-}
path_search_recursively :: [FilePath] -> FilePath -> IO [FilePath]
path_search_recursively p fn =
  case p of
    [] -> return []
    dir : p' -> do
      r <- dir_find fn dir
      r' <- path_search_recursively p' fn
      return (r ++ r')
