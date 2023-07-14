-- | "System.IO" related functions.
module Music.Theory.Io where

import Control.Monad {- base -}
import System.IO {- base -}

import qualified Data.ByteString as B {- bytestring -}
import qualified System.Directory as D {- directory -}

import qualified Control.Monad.Loops as Loop {- monad-loops -}

import qualified Data.Text as T {- text -}
import qualified Data.Text.Encoding as T {- text -}
import qualified Data.Text.IO as T {- text -}

{- | File size, in bytes.

>>> file_size "/home/rohan/sw/hmt-base/Music/Theory/Io.hs"
2643
-}
file_size :: FilePath -> IO Integer
file_size fn = withFile fn ReadMode hFileSize

-- | 'T.decodeUtf8' of 'B.readFile', implemented via "Data.Text".
read_file_utf8_text :: FilePath -> IO T.Text
read_file_utf8_text = fmap T.decodeUtf8 . B.readFile

-- | Read (strictly) a UTF-8 encoded text file, implemented via "Data.Text".
read_file_utf8 :: FilePath -> IO String
read_file_utf8 = fmap T.unpack . read_file_utf8_text

-- | 'read_file_utf8', or a default value if the file doesn't exist.
read_file_utf8_or :: String -> FilePath -> IO String
read_file_utf8_or def f = do
  x <- D.doesFileExist f
  if x then read_file_utf8 f else return def

-- | Write UTF8 string as file, via "Data.Text".
write_file_utf8 :: FilePath -> String -> IO ()
write_file_utf8 fn = B.writeFile fn . T.encodeUtf8 . T.pack

-- | 'readFile' variant using 'T.Text' for @ISO 8859-1@ (Latin 1) encoding.
read_file_iso_8859_1 :: FilePath -> IO String
read_file_iso_8859_1 = fmap (T.unpack . T.decodeLatin1) . B.readFile

-- | 'readFile' variant using 'T.Text' for local encoding.
read_file_locale :: FilePath -> IO String
read_file_locale = fmap T.unpack . T.readFile

-- | Interact with files.  Like Prelude.interact, but with named files.
interactWithFiles :: FilePath -> FilePath -> (String -> String) -> IO ()
interactWithFiles inputFile outputFile process = do
  input <- readFile inputFile
  writeFile outputFile (process input)

-- | Get line from stdin if there is any input, else Nothing.
getLineFromStdinIfReady :: IO (Maybe String)
getLineFromStdinIfReady = do
  r <- hReady stdin
  if r then fmap Just getLine else return Nothing

-- | Wait for input to be available, and then get lines while input remains available.
getAvailableLinesFromStdin :: IO [String]
getAvailableLinesFromStdin = do
  _ <- hWaitForInput stdin (-1)
  Loop.unfoldM getLineFromStdinIfReady

-- | Interact with stdin and stdout.  Like Prelude.interact, but with pipes.
interactWithStdio :: (String -> String) -> IO ()
interactWithStdio strFunc = forever (getAvailableLinesFromStdin >>= \ln -> putStrLn (strFunc (unlines ln)) >> hFlush stdout)
