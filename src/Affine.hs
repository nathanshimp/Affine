module Main where

import Data.Word
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

usage :: String
usage = "usage: affine (encrypt|decrypt|decipher) infile outfile (a b|dictfile)"

tfst :: (a, b, c) -> a
tfst (x, _, _) = x

tsnd :: (a, b, c) -> b
tsnd (_, x, _) = x

ttrd :: (a, b, c) -> c
ttrd (_, _, x) = x

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

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

main :: IO ()
main = do
  args <- getArgs
  input <- C.readFile $ head (tail args)
  let mode = head args
  case mode of "encrypt"  -> C.writeFile output $ encrypt input a b
                                 where output =  head $ tail (tail args)
                                       a = read $ last (init args) :: Integer
                                       b = read $ last args :: Integer
               "decrypt"  -> C.writeFile output $ decrypt input a b
                                 where output =  head $ tail (tail args)
                                       a = read $ last (init args) :: Integer
                                       b = read $ last args :: Integer
               "decipher" -> putStrLn "H"
               _ -> putStrLn usage
