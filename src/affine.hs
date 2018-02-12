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

-- Simple Triple functions
tfst :: (a, b, c) -> a
tfst (x, _, _) = x

tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

ttrd :: (a, b, c) -> c
ttrd (_, _, x) = x

shift :: Int -> Int -> Char -> Char
shift a b x = chr $ (a * x' + b) `mod` 128
  where x' = ord x

unshift :: Int -> Int -> Char -> Char
unshift a b x = chr $ a' * (x' - b) `mod` 128
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

deciphers :: String -> [(String, Int, Int)]
deciphers ciphertext =
  let multipliers = generateMultipliers 128
  in [((decrypt ciphertext a b), a, b) | a <- multipliers, b <- [0..127]]

showDecipher :: (String, Int, Int) -> String
showDecipher decipher = (show . tsnd) decipher
                          ++ " "
                          ++ (show . ttrd) decipher
                          ++ "\n"
                          ++ "DECRYPTED MESSAGE: \n"
                          ++ tfst decipher

decipher :: String -> FilePath -> IO String
decipher ciphertext dictionaryPath = do
  dict <- readFile dictionaryPath
  let dictMap           = buildDictionary dict
  let possibleDeciphers = deciphers ciphertext
  let usableDeciphers   = map (splitOn " " . tfst) possibleDeciphers
  let wordCount         = map (countWords dictMap) usableDeciphers
  let rankedDeciphers   = zip wordCount possibleDeciphers
  let bestDecipher      = snd $ maximum rankedDeciphers
  return $ showDecipher bestDecipher

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp <shimpjn@dukes.jmu.edu>
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

usage :: IO ()
usage = putStrLn "usage: affine (encrypt|decrypt|decipher) input output (a b|dictionary)"

parseDispatch :: [String] -> IO ()
parseDispatch ["encrypt", inFile, outFile, x, y] = do
  input <- readFile inFile
  writeFile outFile $ encrypt input a b
    where a = read x :: Int
          b = read y :: Int
parseDispatch ["decrypt", inFile, outFile, x, y] = do
  input <- readFile inFile
  writeFile outFile $ decrypt input a b
    where a = read x :: Int
          b = read y :: Int
parseDispatch ["decipher", inFile, outFile, dictFile] = do
  input <- readFile inFile
  decipher input dictFile >>= writeFile outFile
parseDispatch _ = usage

main :: IO ()
main = do
  args <- getArgs
  parseDispatch args
