-- Nate Shimp <shimpjn@protonmail.com>
--
-- File: Affine.hs
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

encrypt :: Int -> Int -> B.ByteString -> B.ByteString
encrypt a b plaintext = 
  B.pack $ map (shift a b) $ B.unpack plaintext

decrypt :: Int -> Int -> B.ByteString -> B.ByteString
decrypt a b ciphertext =
  B.pack $ map (unshift a b) $ B.unpack ciphertext

generateMultipliers :: Int -> [Int]
generateMultipliers n = filter (\x -> gcd x n == 1) [1..n]

buildDictionary :: B.ByteString -> M.HashMap B.ByteString Int
buildDictionary dictionary =
  let words = B.split '\n' dictionary
  in M.fromList $ zip words (take (length words) (repeat 0))

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
  in [((decrypt a b ciphertext), a, b) | a <- multipliers, b <- [0..127]]

showDecipher :: (B.ByteString, Int, Int) -> B.ByteString
showDecipher decipher = B.concat [a, (B.singleton ' '), b,
                                  (B.pack "\nDECRYPTED MESSAGE: \n"), (first decipher)]
                                  where a = (B.singleton . chr . second) decipher
                                        b = (B.singleton . chr . third) decipher

decipher :: B.ByteString -> B.ByteString -> B.ByteString
decipher dictionary ciphertext =
  let dictMap              = buildDictionary dictionary
      possibleDeciphers    = deciphers ciphertext
      countableDeciphers   = map (B.split ' ' . first) possibleDeciphers
      wordCount            = map (countWords dictMap) countableDeciphers
      rankedDeciphers      = zip wordCount possibleDeciphers
      bestDecipher         = snd $ maximum rankedDeciphers
  in showDecipher bestDecipher

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp <shimpjn@dukes.jmu.edu>
egcd :: Int -> Int -> (Int, Int, Int)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

usage :: IO ()
usage = putStrLn "usage: affine (encrypt|decrypt|decipher) input output (a b|dictionary)"

invalidKeyPair :: String -> String -> IO ()
invalidKeyPair a b = putStrLn $ "The key pair (" ++ a ++ ", " ++ b
                      ++ ") is invalid, please select another key."

parseDispatch :: [String] -> IO ()
parseDispatch ["encrypt", inFile, outFile, x, y] =
  if gcd a 128 == 1 then
    B.readFile inFile >>= B.writeFile outFile . encrypt a b
  else
    invalidKeyPair x y
  where a = read x :: Int
        b = read y :: Int
parseDispatch ["decrypt", inFile, outFile, x, y] =
  if gcd a 128 == 1 then
    B.readFile inFile >>= B.writeFile outFile . decrypt a b
  else
    invalidKeyPair x y
  where a = read x :: Int
        b = read y :: Int
parseDispatch ["decipher", inFile, outFile, dictFile] = do
  input <- B.readFile inFile
  dict  <- B.readFile dictFile
  B.writeFile outFile $ decipher dict input
parseDispatch _ = usage

main :: IO ()
main = getArgs >>= parseDispatch
