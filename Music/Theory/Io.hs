-- | "System.IO" related functions.
module Music.Theory.Io where

import qualified Control.Monad {- base -}
import qualified System.IO {- base -}

import qualified Data.ByteString {- bytestring -}
import qualified System.Directory {- directory -}

import qualified Control.Monad.Loops {- monad-loops -}

import qualified Data.Text {- text -}
import qualified Data.Text.Encoding {- text -}
import qualified Data.Text.IO {- text -}

import qualified Music.Theory.Directory as Directory {- hmt-base -}

{- | File size, in bytes.

>>> file_size "/home/rohan/sw/hmt-base/Music/Theory/Io.hs"
3599
-}
file_size :: FilePath -> IO Integer
file_size fn = System.IO.withFile fn System.IO.ReadMode System.IO.hFileSize

-- | 'Data.Text.decodeUtf8' of 'Data.ByteString.readFile', implemented via "Data.Text".
read_file_utf8_text :: FilePath -> IO Data.Text.Text
read_file_utf8_text = fmap Data.Text.Encoding.decodeUtf8 . Data.ByteString.readFile

-- | Read (strictly) a Utf-8 encoded text file, implemented via "Data.Text".
read_file_utf8 :: FilePath -> IO String
read_file_utf8 = fmap Data.Text.unpack . read_file_utf8_text

-- | 'read_file_utf8', or a default value if the file doesn't exist.
read_file_utf8_or :: String -> FilePath -> IO String
read_file_utf8_or def f = do
  x <- System.Directory.doesFileExist f
  if x then read_file_utf8 f else return def

-- | If fn is an existing file then print that it exists, else run action x.
when_file_does_not_exist :: FilePath -> IO () -> IO ()
when_file_does_not_exist fn x = Directory.if_file_exists (fn, print ("File exists", fn), x)

-- | Write text file only if it does not exist already.
write_text_file_x :: FilePath -> String -> IO ()
write_text_file_x fn txt = when_file_does_not_exist fn (writeFile fn txt)

-- | Write Utf8 string as file, via "Data.Text".
write_file_utf8 :: FilePath -> String -> IO ()
write_file_utf8 fn =
  Data.ByteString.writeFile fn
    . Data.Text.Encoding.encodeUtf8
    . Data.Text.pack

-- | 'readFile' variant using 'Data.Text.Text' for @ISO 8859-1@ (Latin 1) encoding.
read_file_iso_8859_1 :: FilePath -> IO String
read_file_iso_8859_1 =
  fmap (Data.Text.unpack . Data.Text.Encoding.decodeLatin1)
    . Data.ByteString.readFile

-- | 'readFile' variant using 'Data.Text.Text' for local encoding.
read_file_locale :: FilePath -> IO String
read_file_locale =
  fmap Data.Text.unpack
    . Data.Text.IO.readFile

-- | Interact with files.  Like Prelude.interact, but with named files.
interactWithFiles :: FilePath -> FilePath -> (String -> String) -> IO ()
interactWithFiles inputFile outputFile process = do
  input <- readFile inputFile
  writeFile outputFile (process input)

-- | Get line from stdin if there is any input, else Nothing.
getLineFromStdinIfReady :: IO (Maybe String)
getLineFromStdinIfReady = do
  r <- System.IO.hReady System.IO.stdin
  if r then fmap Just getLine else return Nothing

-- | Wait for input to be available, and then get lines while input remains available.
getAvailableLinesFromStdin :: IO [String]
getAvailableLinesFromStdin = do
  _ <- System.IO.hWaitForInput System.IO.stdin (-1)
  Control.Monad.Loops.unfoldM getLineFromStdinIfReady

-- | Interact with stdin and stdout.  Like Prelude.interact, but with pipes.
interactWithStdio :: (String -> String) -> IO ()
interactWithStdio strFunc =
  Control.Monad.forever
    ( getAvailableLinesFromStdin
        >>= \ln ->
          putStrLn (strFunc (unlines ln))
            >> System.IO.hFlush System.IO.stdout
    )

-- | Interact with lines
interactLines :: ([String] -> [String]) -> IO ()
interactLines f = interact (unlines . f . lines)
