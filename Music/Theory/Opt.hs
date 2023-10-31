{- | Very simple command line interface option parser.

Only allows options of the form --key=value, with the form --key equal to --key=True.

A list of OptUsr describes the options and provides default values.

'get_opt_arg' merges user and default values into a table with values for all options.
It also consults the environment.

To fetch options use 'opt_get' and 'opt_read'.

-}
module Music.Theory.Opt where

import Control.Monad {- base -}
import Data.List {- base -}
import Data.Maybe {- base -}
import System.Environment {- base -}
import System.Exit {- base -}

import qualified Data.List.Split as Split {- split -}

import qualified Music.Theory.Either as Either {- hmt-base -}
import qualified Music.Theory.Read as Read{- hmt-base -}

{- | (Key,Value)

Key does not include leading '--'.
-}
type Opt = (String,String)

-- | (Key,Default-Value,Type,Note)
type OptUsr = (String,String,String,String)

-- | Re-write default values at OptUsr.
opt_usr_rw_def :: [Opt] -> [OptUsr] -> [OptUsr]
opt_usr_rw_def rw =
  let f (k,v,ty,dsc) =
        case lookup k rw of
          Just v' -> (k,v',ty,dsc)
          Nothing -> (k,v,ty,dsc)
  in map f

-- | OptUsr to Opt.
opt_plain :: OptUsr -> Opt
opt_plain (k,v,_,_) = (k,v)

-- | OptUsr to help string, indent is two spaces.
opt_usr_help :: OptUsr -> String
opt_usr_help (k,v,t,n) = concat ["  ",k,":",t," -- ",n,"; default=",if null v then "Nil" else v]

-- | 'unlines' of 'opt_usr_help'
opt_help :: [OptUsr] -> String
opt_help = unlines . map opt_usr_help

-- | Lookup Key in Opt, error if non-existing.
opt_get :: [Opt] -> String -> String
opt_get o k = fromMaybe (error ("opt_get: " ++ k)) (lookup k o)

-- | Variant that returns Nothing if the result is the empty string, else Just the result.
opt_get_nil :: [Opt] -> String -> Maybe String
opt_get_nil o k = let r = opt_get o k in if null r then Nothing else Just r

-- | 'read' of 'get_opt'
opt_read :: Read t => [Opt] -> String -> t
opt_read o = Read.read_err . opt_get o

-- | Parse k or k=v string, else error.
opt_param_parse :: String -> Opt
opt_param_parse p =
  case Split.splitOn "=" p of
    [lhs] -> (lhs,"True")
    [lhs,rhs] -> (lhs,rhs)
    _ -> error ("opt_param_parse: " ++ p)

{- | Parse option string of form "--opt" or "--key=value".

>>> opt_parse "--opt"
Just ("opt","True")

>>> opt_parse "--key=value"
Just ("key","value")
-}
opt_parse :: String -> Maybe Opt
opt_parse s =
  case s of
    '-':'-':p -> Just (opt_param_parse p)
    _ -> Nothing

{- | Parse option sequence, collating options and non-options.

>>> opt_set_parse (words "--a --b=c d")
([("a","True"),("b","c")],["d"])
-}
opt_set_parse :: [String] -> ([Opt],[String])
opt_set_parse =
  let f s = maybe (Right s) Left (opt_parse s)
  in Either.partition_eithers . map f

-- | Left-biased Opt merge.
opt_merge :: [Opt] -> [Opt] -> [Opt]
opt_merge p q =
  let x = map fst p
  in p ++ filter (\(k,_) -> k `notElem` x) q

-- | Process argument list.
opt_proc :: [OptUsr] -> [String] -> ([Opt], [String])
opt_proc def arg =
  let (o,a) = opt_set_parse arg
  in (opt_merge o (map opt_plain def),a)

-- | Usage text
type OptHelp = [String]

-- | Format usage pre-amble and 'opt_help'.
opt_help_pp :: OptHelp -> [OptUsr] -> String
opt_help_pp usg def = unlines (usg ++ ["",opt_help def])

-- | Print help and exit.
opt_usage :: OptHelp -> [OptUsr] -> IO ()
opt_usage usg def = putStrLn (opt_help_pp usg def)  >> exitSuccess

-- | Print help and error.
opt_error :: OptHelp -> [OptUsr] -> t
opt_error usg def = error (opt_help_pp usg def)

-- | Verify that all Opt have keys that are in OptUsr
opt_verify :: OptHelp -> [OptUsr] -> [Opt] -> IO ()
opt_verify usg def =
  let k_set = map (fst . opt_plain) def
      f (k,_) = if k `elem` k_set
                then return ()
                else putStrLn ("Unknown Key: " ++ k ++ "\n") >> opt_usage usg def
  in mapM_ f

{- | Process arguments consulting environment.
Options are selected over environment values, which are selected over default values.

>>> opt_proc_arg [("x","0","number","X")] ["--x=1","y"] []
([("x","1")],[("x","1")],["y"])

>>> opt_proc_arg [("x","0","number","X")] ["y"] [("x","1")]
([],[("x","1")],["y"])

>>> opt_proc_arg [("x","0","number","X")] ["y"] []
([],[("x","0")],["y"])

>>> opt_proc_arg [("x","0","number","X")] ["--x=a","y"] [("x","e")]
([("x","a")],[("x","a")],["y"])
-}
opt_proc_arg :: [OptUsr] -> [String] -> [(String,String)] -> ([Opt],[Opt],[String])
opt_proc_arg def arg env =
  let u = map (\(k,_,_,_) -> k) def
      env' = filter (\(k,_) -> k `elem` u) env
      (o,p) = opt_set_parse arg
      o' = opt_merge (opt_merge o env') (map opt_plain def)
  in (o,o',p)

-- | 'opt_set_parse' and maybe 'opt_verify' and 'opt_merge' of 'getArgs'.
--   If arguments include -h or --help run 'opt_usage'
opt_get_arg :: Bool -> OptHelp -> [OptUsr] -> IO ([Opt],[String])
opt_get_arg chk usg def = do
  arg <- getArgs
  env <- getEnvironment
  when ("-h" `elem` arg || "--help" `elem` arg) (opt_usage usg def)
  let (o,o',p) = opt_proc_arg def arg env
  when chk (opt_verify usg def o)
  return (o',p)

{- | Parse param set, one parameter per line.

>>> opt_param_set_parse "a\nb=c"
[("a","True"),("b","c")]
-}
opt_param_set_parse :: String -> [Opt]
opt_param_set_parse = map opt_param_parse . lines

-- | Simple scanner over argument list.
opt_scan :: [String] -> String -> Maybe String
opt_scan a k =
  let (o,_) = opt_set_parse a
  in fmap snd (find ((== k) . fst) o)

-- | Scanner with default value.
opt_scan_def :: [String] -> (String,String) -> String
opt_scan_def a (k,v) = fromMaybe v (opt_scan a k)

-- | Reading scanner with default value.
opt_scan_read :: Read t => [String] -> (String,t) -> t
opt_scan_read a (k,v) = maybe v read (opt_scan a k)
