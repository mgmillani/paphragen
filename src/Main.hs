--  Copyright 2016 Marcelo Garlet Millani
--  This file is part of paphragen.

--  paphragen is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  paphragen is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with paphragen.  If not, see <http://www.gnu.org/licenses/>.


module Main where

import Data.Char
import Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.Word as W
import qualified Data.Bits as B
import qualified Data.Map.Strict as Map
import System.Environment
import System.IO

appname = "paphragen"
appversion = "0.1.0.0"

data Command = Help | Build | Generate deriving(Eq, Show, Ord)
data Action =
  Action
  { cmd         :: Command
  , outFile     :: FilePath
  , passLength  :: Int
  , passEntropy :: Int
  , inputFiles  :: [FilePath]
  }

defaultAction =
  Action
  { cmd         = Help
  , outFile     = ""
  , passLength  = 0
  , passEntropy = 100
  , inputFiles  = []
  }

wordSplit [] = []
wordSplit text =
  (map toLower word) : (wordSplit rest)
  where
    (word, text') = span isLetter text
    rest = dropWhile (not . isLetter) text'

writeDictionary dict handle = do
  mapM_ (hPutStrLn handle) $ map fst $ Map.toAscList dict

loadDictionary fl = do
  text <- readFile fl
  return $ lines text

insertWords dict text = foldl (\d w -> Map.insertWith (+) w 1 d) dict $ wordSplit text

readDictionary dict [] = return dict
readDictionary dict (f:fs) = do
  text <- readFile f
  readDictionary (insertWords dict text) fs

buildDictionary files out = do
  rawDict <- readDictionary Map.empty files
  let dictionary = Map.filterWithKey (\w c -> length w > 1 && length w < 8) $ rawDict
      (distinct, occurrences) = Map.foldl (\(d,o) y -> (d+1, o+y)) (0,0) dictionary
      threshold  = occurrences `div` (distinct * 3)
      common     = Map.filter (>= threshold) dictionary
  writeDictionary common out

takeIndices is ds =
  takeIndices' 0 is ds
  where
    takeIndices' k [] ds = []
    takeIndices' k ((x,i):is) ds = (head rs, i) : takeIndices' x is rs
      where
        rs = drop (x-k) ds

-- | Generates indices for a set of n elements using r as a source of randomness
randomIndices n rs
  | x < n     = x : randomIndices n (BS.drop bytes rs)
  | otherwise = randomIndices n (BS.drop bytes rs)
  where
    entropy = ceiling $ logBase 2 (fromIntegral n)
    bytes = ceiling $ logBase 8 (fromIntegral n)
    mask = 2^entropy - 1
    x = mask B..&. (fromIntegral $ BS.foldl accum (0 :: W.Word) (BS.take bytes rs))
    accum b x = (B.shift b 8) B..|. (fromIntegral x)

help = do
  mapM_ putStrLn [
      appname ++ " " ++ appversion
    , "A passphrase generator."
    ,  "usage:"
    , "To build a dictionary based on the words of input files:"
    , "\t" ++ appname ++ " build [OPTIONS...] <TEXT_FILE...>"
    , "\t  where OPTIONS are:"
    , "\t\t-o, --output DICTIONARY     writes output to DICTIONARY instead of stdout."
    , "\nTo generate a password using an existing dictionary:"
    , "\t" ++ appname ++ " generate [OPTIONS...] <DICTIONARY...>"
    , "\t  where OPTIONS are:"
    , "\t\t-e, --entropy N      sets the minimum desired entropy (default: " ++ show (passEntropy defaultAction) ++ " bits)."
    , "\t\t-l, --length N       number of words to use (entropy is used by default)."
    , "\tIn this case, a random sequence of bytes should be provided through stdin."
    , "\tOn Unix-like systems, /dev/random is a good choice."
    ]

parseArgs action args
  | cmd action == Build = case args of
    "-o":file:rs -> parseArgs action{outFile = file} rs
    "--output":file:rs -> parseArgs action{outFile = file} rs
    rs -> action{inputFiles = rs}
  | cmd action == Generate = case args of
    "-l":len:rs  -> parseArgs action{passLength  = read len} rs
    "--length":len:rs  -> parseArgs action{passLength  = read len} rs
    "-e":ent:rs  -> parseArgs action{passEntropy = read ent} rs
    "--entropy":ent:rs  -> parseArgs action{passEntropy = read ent} rs
    rs -> action{inputFiles = rs}
  | cmd action == Help = case args of
    "build":rs -> parseArgs action{cmd = Build} rs
    "generate":rs -> parseArgs action{cmd = Generate} rs
    rs -> parseOptions action rs
parseOptions action args = case args of
  "-o":file:rs -> parseArgs action{outFile    = file}     rs
  "--output":file:rs -> parseArgs action{outFile    = file}     rs
  "-l":len:rs  -> parseArgs action{passLength  = read len} rs
  "--length":len:rs  -> parseArgs action{passLength  = read len} rs
  "-e":ent:rs  -> parseArgs action{passEntropy = read ent} rs
  "--entropy":ent:rs  -> parseArgs action{passEntropy = read ent} rs
  _ -> action{cmd = Help}

execute action
  | cmd action == Build    = execBuild action
  | cmd action == Generate = execGenerate action
  | cmd action == Help     = help
execBuild action
  | null $ outFile action = do
    buildDictionary (inputFiles action) stdout
  | otherwise = do
    withFile (outFile action) WriteMode (buildDictionary (inputFiles action))
    putStrLn $ "Common words written to " ++ (outFile action)
execGenerate action
  | null $ inputFiles action = execute action{cmd = Help}
  | otherwise = do
    dict <- (mapM loadDictionary $ inputFiles action) >>= (return . concat)
    let n = length dict
    putStrLn $ "Dictionary has " ++ show n ++ " words."
    let epw = logBase 2 (fromIntegral n)
    putStrLn $ "This gives an entropy of " ++ show epw ++ " bits per word."
    let k = if passLength action == 0 then ceiling $ (fromIntegral $ passEntropy action) / epw else passLength action
    putStrLn $ "Generating a password with " ++ show k ++ " words (" ++ show ((fromIntegral k) * epw) ++ " bits of entropy)."
    randomness <- BS.getContents
    let indices = zip (take k $ randomIndices n randomness) [0..]
        sInd = sortBy (\x y -> (fst x) `compare` (fst y)) indices
        pickedWords = sortBy (\x y -> (snd x) `compare` (snd y)) $ takeIndices sInd dict
    mapM_ (\w -> putStr $ " " ++ fst w) pickedWords
    putStr "\n"

main :: IO ()
main = do
  args <- getArgs
  let action = parseArgs defaultAction args
  execute action

