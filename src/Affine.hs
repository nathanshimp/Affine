-- Nate Shimp <shimpjn@dukes.jmu.edu>
--
-- File: affine.hs
module Main where

import Data.Ord
import Data.Char
import Data.List
import Data.List.Split
import System.Exit
import System.Environment
import qualified Data.HashMap.Lazy as M

tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

shift :: Int -> Int -> Char -> Char
shift a b x = chr $ (a * x' + b) `mod` 128
  where x' = ord x

unshift :: Int -> Int -> Char -> Char
unshift a b x = chr $ (a * x' + b) `mod` 128
  where a' = tsnd (egcd a 128)
        x' = ord x

encrypt :: String -> Int -> Int -> String
encrypt plaintext a b = 
  map (shift a b) plaintext

decrypt :: String -> Int -> Int -> String
decrypt ciphertext a b =
  map (unshift a b) ciphertext

generateMultipliers :: Int -> [Int]
generateMultipliers n = filter (\x -> gcd x n == 1) [1..n]

buildDictionary :: String -> M.HashMap String Int
buildDictionary dictionary = do
  let words = lines dictionary
  M.fromList $ zip words (take (length words) (repeat 0))

countWords :: M.HashMap String Int -> [String] -> Int
countWords dictionary words = 
  let hits = map ((flip M.member) dictionary) words
  in  length $ filter id hits 

decipher :: String -> FilePath -> IO String
decipher ciphertext dictionaryPath = do
  dict <- readFile dictionaryPath
  let dictMap = buildDictionary dict
  let possibleDeciphers = map (splitOn " ") (deciphers ciphertext)
  let wordCount = map (countWords dictMap) possibleDeciphers
  let rankedDeciphers = zip wordCount possibleDeciphers
  let bestDecipher = snd $ maximum rankedDeciphers
  return $ intercalate " " bestDecipher
  
deciphers :: String -> [String]
deciphers ciphertext =
  (map (decrypt ciphertext) (generateMultipliers 128)) <*> [0..127]

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp <shimpjn@dukes.jmu.edu>
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

usage :: IO ()
usage = putStrLn "usage: affine (encrypt|decrypt|decipher) input output (a b|dictionary)"

exit = exitWith ExitSuccess
die  = exitWith $ ExitFailure 1

main :: IO ()
main = do
  args <- getArgs

  let mode:inFile:outFile:deps = args
  input <- readFile inFile

  case mode of
    "encrypt" -> writeFile outFile $ encrypt input a b
                    where a = read $ head deps :: Int
                          b = read $ last deps :: Int
    "decrypt" -> writeFile outFile $ decrypt input a b
                   where a = read $ head deps :: Int
                         b = read $ last deps :: Int
    "decipher" -> decipher input dict >>= writeFile outFile
                    where dict = head deps
    otherwise -> usage
