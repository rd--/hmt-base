-- | Directory functions.
module Music.Theory.Directory where

import qualified Control.Monad {- base -}
import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}
import qualified System.Environment {- base -}

import qualified Data.List.Split {- split -}
import qualified System.Directory {- directory -}
import qualified System.FilePath {- filepath -}

import qualified Music.Theory.Monad as Monad {- hmt-base -}

{- | 'takeDirectory' gives different answers depending on whether there is a trailing separator.

>>> let x = ["x/y","x/y/","x","/"]
>>> map parent_dir x
["x","x",".","/"]

>>> map System.FilePath.takeDirectory x
["x","x/y",".","/"]
-}
parent_dir :: FilePath -> FilePath
parent_dir = System.FilePath.takeDirectory . System.FilePath.dropTrailingPathSeparator

-- | Colon separated path list.
path_split :: String -> [FilePath]
path_split = Data.List.Split.splitOn ":"

{- | Read environment variable and split path.
     Error if enviroment variable not set.

> path_from_env "PATH"
> path_from_env "NONPATH" -- error
-}
path_from_env :: String -> IO [FilePath]
path_from_env k = do
  p <- System.Environment.lookupEnv k
  maybe (error ("Environment variable not set: " ++ k)) (return . path_split) p

{- | Expand a path to include all subdirectories recursively.

>>> let p = ["/home/rohan/sw/hmt-base/Music", "/home/rohan/sw/hmt/Music"]
>>> r <- path_recursive p
>>> length r
59
-}
path_recursive :: [FilePath] -> IO [FilePath]
path_recursive p = do
  p' <- mapM dir_subdirs_recursively p
  return (p ++ concat p')

{- | Scan a list of directories until a file is located, or not.
Stop once a file is located, do not traverse any sub-directory structure.

>>> mapM (path_scan ["/sbin","/usr/bin"]) ["fsck","ghc"]
[Just "/sbin/fsck",Nothing]
-}
path_scan :: [FilePath] -> FilePath -> IO (Maybe FilePath)
path_scan p fn =
  case p of
    [] -> return Nothing
    dir : p' ->
      let nm = dir System.FilePath.</> fn
          f x = if x then return (Just nm) else path_scan p' fn
      in System.Directory.doesFileExist nm >>= f

-- | Erroring variant.
path_scan_err :: [FilePath] -> FilePath -> IO FilePath
path_scan_err p x =
  let err = error (concat ["path_scan: ", show p, ": ", x])
  in fmap (Data.Maybe.fromMaybe err) (path_scan p x)

{- | Scan a list of directories and return all located files.
Do not traverse any sub-directory structure.
Since 1.2.1.0 there is also findFiles.

>>> let path = ["/home/rohan/sw/hmt-base","/home/rohan/sw/hmt"]
>>> path_search path "README.md"
["/home/rohan/sw/hmt-base/README.md","/home/rohan/sw/hmt/README.md"]

>>> System.Directory.findFiles path "README.md"
["/home/rohan/sw/hmt-base/README.md","/home/rohan/sw/hmt/README.md"]
-}
path_search :: [FilePath] -> FilePath -> IO [FilePath]
path_search p fn = do
  let fq = map (\dir -> dir System.FilePath.</> fn) p
      chk q = System.Directory.doesFileExist q >>= \x -> return (if x then Just q else Nothing)
  fmap Data.Maybe.catMaybes (mapM chk fq)

{- | Get sorted list of files at /dir/ with /ext/, ie. ls dir/*.ext.
The exact rule is that ext must be a suffix of the complete extension.
I.e. requesting .sl will match files having extension .help.sl,
but naturally requesting .help.sl will not match files with only the extension .sl.

> dir_list_ext "/home/rohan/rd/j/" ".hs"
> dir_list_ext "/home/rohan/sw/spl/Help/Guide/" ".help.sl"
-}
dir_list_ext :: FilePath -> String -> IO [FilePath]
dir_list_ext dir ext = do
  l <- System.Directory.listDirectory dir
  let fn = filter (\x -> ext `Data.List.isSuffixOf` System.FilePath.takeExtensions x) l
  return (Data.List.sort fn)

{- | Post-process 'dir_list_ext' to gives file-names with /dir/ prefix.

> dir_list_ext_path "/home/rohan/rd/j/" ".hs"
-}
dir_list_ext_path :: FilePath -> String -> IO [FilePath]
dir_list_ext_path dir ext = fmap (map (dir System.FilePath.</>)) (dir_list_ext dir ext)

{- | Subset of files in /dir/ with an extension in /ext/.
  Extensions include the leading dot and are case-sensitive.
  Results are relative to /dir/.
-}
dir_subset_rel :: [String] -> FilePath -> IO [FilePath]
dir_subset_rel ext dir = do
  let f nm = System.FilePath.takeExtensions nm `elem` ext
  c <- System.Directory.getDirectoryContents dir
  return (Data.List.sort (filter f c))

{- | Variant of dir_subset_rel where results have dir/ prefix.

> dir_subset [".hs"] "/home/rohan/sw/hmt/cmd"
-}
dir_subset :: [String] -> FilePath -> IO [FilePath]
dir_subset ext dir = fmap (map (dir System.FilePath.</>)) (dir_subset_rel ext dir)

-- | Subdirectories (relative) of /dir/.
dir_subdirs_rel :: FilePath -> IO [FilePath]
dir_subdirs_rel dir =
  let sel fn = System.Directory.doesDirectoryExist (dir System.FilePath.</> fn)
  in System.Directory.listDirectory dir >>= Control.Monad.filterM sel

-- | Subdirectories of /dir/.
dir_subdirs :: FilePath -> IO [FilePath]
dir_subdirs dir = fmap (map (dir System.FilePath.</>)) (dir_subdirs_rel dir)

{- | Recursive form of 'dir_subdirs'.

> dir_subdirs_recursively "/home/rohan/sw/hmt-base/Music"
-}
dir_subdirs_recursively :: FilePath -> IO [FilePath]
dir_subdirs_recursively dir = do
  subdirs <- dir_subdirs dir
  case subdirs of
    [] -> return []
    _ -> do
      subdirs' <- mapM dir_subdirs_recursively subdirs
      return (subdirs ++ concat subdirs')

{- | If path is not absolute, prepend current working directory.

> to_absolute_cwd "x"
-}
to_absolute_cwd :: FilePath -> IO FilePath
to_absolute_cwd x =
  if System.FilePath.isAbsolute x
    then return x
    else fmap (System.FilePath.</> x) System.Directory.getCurrentDirectory

-- | If /i/ is an existing file then /j/ else /k/.
if_file_exists :: (FilePath, IO t, IO t) -> IO t
if_file_exists (i, j, k) = Monad.m_if (System.Directory.doesFileExist i, j, k)

-- | 'createDirectoryIfMissing' (including parents) and then 'writeFile'
writeFile_mkdir :: FilePath -> String -> IO ()
writeFile_mkdir fn s = do
  let dir = System.FilePath.takeDirectory fn
  System.Directory.createDirectoryIfMissing True dir
  writeFile fn s

-- | 'writeFile_mkdir' only if file does not exist.
writeFile_mkdir_x :: FilePath -> String -> IO ()
writeFile_mkdir_x fn txt = if_file_exists (fn, return (), writeFile_mkdir fn txt)
