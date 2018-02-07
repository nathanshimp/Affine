module Main where

import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

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

-- decipher :: B.ByteString -> B.ByteString -> B.ByteString
-- decipher ciphertext dictionary = 

-- Source: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
egcd :: Integer -> Integer -> (Integer, Integer, Integer)
egcd 0 b = (b, 0, 1)
egcd a b = let (d, s, t) = egcd (b `mod` a) a
           in (d, t - (b `div` a) * s, s)

usage :: IO ()
usage = putStrLn "usage: affine (encrypt|decrypt|decipher) inFile outFile (a b|dictfile)"

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
    -- "decipher" -> C.writeFile outFile $ decipher input dict
    --                 where dict = head deps 
    otherwise -> usage
