module Main where

import Data.Ord
import Data.List
import System.Exit
import System.Environment
import qualified Data.HashMap.Lazy as H
import qualified Data.ByteString.Lazy as B

tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

encrypt :: B.ByteString -> Int -> Int -> B.ByteString
encrypt plaintext a b = 
  B.pack $ map (\x -> (a' * x + b') `mod` 128) (B.unpack plaintext)
    where a' = fromIntegral a
          b' = fromIntegral b

decrypt :: B.ByteString -> Int -> Int -> B.ByteString
decrypt ciphertext a b =
  B.pack $ map (\x -> (a' * (x - b')) `mod` 128) (B.unpack ciphertext)
    where a' = fromIntegral $ tsnd (egcd a 128)
          b' = fromIntegral b

generateMultipliers :: Int -> [Int]
generateMultipliers n = filter (\x -> gcd x n == 1) [1..n]

dropBadDeciphers :: B.ByteString -> [([B.ByteString], Int)] -> [([B.ByteString], Int)]
dropBadDeciphers ciphertext possibleCiphers =
  dropWhile (\(x, y) -> y < pl) possibleCiphers 
    where pl = length (B.unpack ciphertext) `div` 6

buildDictionary :: B.ByteString -> H.HashMap B.ByteString Int
buildDictionary dictionary = do
  let d = B.split 10 dictionary -- split by new lines '\n'
  H.fromList $ zip d (take (length d) (repeat 0))

countWords :: H.HashMap B.ByteString Int -> [B.ByteString] -> Int
countWords dictionary []   = sum $ H.elems dictionary
countWords dictionary text = 
  let word:words = text
  in countWords (H.update (\x -> Just (x + 1)) word dictionary) words

decipher :: B.ByteString -> FilePath -> IO B.ByteString
decipher ciphertext dictionaryPath = do
  dict <- B.readFile dictionaryPath
  let dictMap = buildDictionary dict
  let possibleDeciphers = map (B.split 32) (deciphers ciphertext)
  let wordCount = map (countWords dictMap) possibleDeciphers
  let rankedDeciphers = zip wordCount possibleDeciphers
  let bestDecipher = snd $ maximum rankedDeciphers
  return $ B.intercalate (B.pack [32]) bestDecipher
  
deciphers :: B.ByteString -> [B.ByteString]
deciphers ciphertext =
  (map (decrypt ciphertext) (generateMultipliers 128)) <*> [0..127]

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
-- Modified by Nate Shimp
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
  input <- B.readFile inFile

  case mode of
    "encrypt" -> B.writeFile outFile $ encrypt input a b
                    where a = read $ head deps :: Int
                          b = read $ last deps :: Int
    "decrypt" -> B.writeFile outFile $ decrypt input a b
                   where a = read $ head deps :: Int
                         b = read $ last deps :: Int
    "decipher" -> decipher input dict >>= B.writeFile outFile
                    where dict = head deps
    otherwise -> usage
