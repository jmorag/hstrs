module Main where

import           Options
import           Control.Monad
import           Data.Int
import qualified Data.ByteString.Lazy.Char8    as B
import           Text.Printf

main :: IO ()
main = run =<< parseOpts

run :: Options -> IO ()
run Version = putStrLn ("hstrs " <> version)
run args    = forM_ (files args) $ \file -> do
  contents <- B.readFile file
  mapM_ (B.putStrLn . format) (strs (number args) contents)
 where
  format (o, str) = B.pack (formatOffset o) <> str
  formatOffset = case offset args of
    Nothing          -> const ""
    Just Decimal     -> printf "%d\t"
    Just Octal       -> printf "%o\t"
    Just Hexadecimal -> printf "%x\t"

strs :: Int64 -> B.ByteString -> [(Int64, B.ByteString)]
strs n bytes =
  let groups  = B.groupBy (\c1 c2 -> printable c1 == printable c2) bytes
      offsets = addOffsets 0 groups
  in  filter (\(_, cs) -> printable (B.head cs) && B.length cs >= n) offsets

printable :: Char -> Bool
printable byte = byte >= '!' && byte <= '~'

addOffsets :: Int64 -> [B.ByteString] -> [(Int64, B.ByteString)]
addOffsets _     []       = []
addOffsets count (g : gs) = (count, g) : addOffsets (count + B.length g) gs
