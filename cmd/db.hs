import System.Environment {- base -}

import Music.Theory.Db.Cli {- hmt-base -}

main :: IO ()
main = getArgs >>= db_cli
