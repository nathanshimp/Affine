module Main where

import Data.Ord
import Data.List
import System.Exit
import System.Environment
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

encrypt :: B.ByteString -> Integer -> Integer -> B.ByteString
encrypt plaintext a b = 
  B.pack $ map (\x -> (a' * x + b') `mod` 128) (B.unpack plaintext)
    where a' = fromIntegral a
          b' = fromIntegral b

decrypt :: B.ByteString -> Integer -> Integer -> B.ByteString
decrypt ciphertext a b =
  B.pack $ map (\x -> (a' * (x - b')) `mod` 128) (B.unpack ciphertext)
    where a' = fromIntegral $ tsnd (egcd a 128)
          b' = fromIntegral b

generateMultipliers :: Integer -> [Integer]
generateMultipliers n = filter (\x -> gcd x n == 1) [1..n]

buildDictionary :: B.ByteString -> Map.Map B.ByteString Integer
buildDictionary dictionary = do
  let d = B.split 10 dictionary -- split by new lines '\n'
  Map.fromList $ zip d (take (length d) (repeat 0))

countWords :: Map.Map B.ByteString Integer -> [B.ByteString] -> Integer
countWords dictionary []   = sum $ Map.elems dictionary
countWords dictionary text = 
  let word:words = text
  in countWords (Map.update (\x -> Just (x + 1)) word dictionary) words

-- decipher :: B.ByteString -> FilePath -> Maybe B.ByteString
decipher ciphertext dictionaryPath = do
  dict <- C.readFile dictionaryPath
  let dictMap = buildDictionary dict
  let possibleDeciphers = map (B.split 32) (deciphers ciphertext)
  let rankedDeciphers = dropWhile (\(x, y) -> y < 10) $ zip possibleDeciphers $ map (countWords dictMap) possibleDeciphers
  let bestDecipher = fst $ maximumBy (comparing snd) rankedDeciphers
  return $ B.intercalate (B.pack [32]) bestDecipher
  
deciphers :: B.ByteString -> [B.ByteString]
deciphers ciphertext =
  (map (decrypt ciphertext) (generateMultipliers 128)) <*> [0..127]

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
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
  input <- C.readFile inFile

  case mode of
    "encrypt" -> C.writeFile outFile $ encrypt input a b
                    where a = read $ head deps :: Integer
                          b = read $ last deps :: Integer
    "decrypt" -> C.writeFile outFile $ decrypt input a b
                   where a = read $ head deps :: Integer
                         b = read $ last deps :: Integer
    "decipher" -> decipher input dict >>= C.writeFile outFile
                    where dict = head deps
    otherwise -> usage
