-- Nate Shimp <shimpjn@dukes.jmu.edu>
--
-- File: affine.hs
module Main where

import Data.Ord
import Data.Char
import System.Environment
import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy.Char8 as B

-- Simple Triple functions
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
    
shift :: Int -> Int -> Char -> Char
shift a b x = chr $ (a * x' + b) `mod` 128
  where x' = ord x

unshift :: Int -> Int -> Char -> Char
unshift a b x = chr $ a' * (x' - b) `mod` 128
  where a' = second (egcd a 128)
        x' = ord x

encrypt :: B.ByteString -> Int -> Int -> B.ByteString
encrypt plaintext a b = 
  B.pack $ map (shift a b) $ B.unpack plaintext

decrypt :: B.ByteString -> Int -> Int -> B.ByteString
decrypt ciphertext a b =
  B.pack $ map (unshift a b) $ B.unpack ciphertext

generateMultipliers :: Int -> [Int]
generateMultipliers n = filter (\x -> gcd x n == 1) [1..n]

buildDictionary :: B.ByteString -> M.HashMap B.ByteString Int
buildDictionary dictionary = do
  let words = B.split '\n' dictionary
  M.fromList $ zip words (take (length words) (repeat 0))

lowcase :: B.ByteString -> B.ByteString
lowcase bstring = B.map toLower bstring
  
countWords :: M.HashMap B.ByteString Int -> [B.ByteString] -> Int
countWords dictionary words = 
  let hits = map ((flip M.member) dictionary) lowWords
  in  length $ filter id hits 
  where lowWords = map lowcase words

deciphers :: B.ByteString -> [(B.ByteString, Int, Int)]
deciphers ciphertext =
  let multipliers = generateMultipliers 128
  in [((decrypt ciphertext a b), a, b) | a <- multipliers, b <- [0..127]]

showDecipher :: (B.ByteString, Int, Int) -> String
showDecipher decipher = (show . second) decipher
                          ++ " "
                          ++ (show . third) decipher
                          ++ "\n"
                          ++ "DECRYPTED MESSAGE: \n"
                          ++ (B.unpack . first) decipher

decipher :: B.ByteString -> FilePath -> IO String
decipher ciphertext dictionaryPath = do
  dict <- B.readFile dictionaryPath
  let dictMap              = buildDictionary dict
  let possibleDeciphers    = deciphers ciphertext
  let countableDeciphers   = map (B.split ' ' . first) possibleDeciphers
  let wordCount            = map (countWords dictMap) countableDeciphers
  let rankedDeciphers      = zip wordCount possibleDeciphers
  let bestDecipher         = snd $ maximum rankedDeciphers
  return $ showDecipher bestDecipher

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp <shimpjn@dukes.jmu.edu>
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

usage :: IO ()
usage = putStrLn "usage: affine (encrypt|decrypt|decipher) input output (a b|dictionary)"

invalidKeyPair :: String -> String -> IO ()
invalidKeyPair a b = putStrLn $ "The key pair ("
                      ++ a ++ ", " ++ b
                      ++ ") is invalid, please select another key."

parseDispatch :: [String] -> IO ()
parseDispatch ["encrypt", inFile, outFile, x, y] = do
  input <- B.readFile inFile
  if gcd a 128 == 1 then
    B.writeFile outFile $ encrypt input a b
  else
    invalidKeyPair x y
  where a = read x :: Int
        b = read y :: Int
parseDispatch ["decrypt", inFile, outFile, x, y] = do
  input <- B.readFile inFile
  if gcd a 128 == 1 then
    B.writeFile outFile $ decrypt input a b
  else
    invalidKeyPair x y
  where a = read x :: Int
        b = read y :: Int
parseDispatch ["decipher", inFile, outFile, dictFile] = do
  input <- B.readFile inFile
  decipher input dictFile >>= writeFile outFile
parseDispatch _ = usage

main :: IO ()
main = do
  args <- getArgs
  parseDispatch args
