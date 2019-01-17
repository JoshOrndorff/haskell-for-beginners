module CiphR
( caesarEnc
, caesarDec
, otpEnc
, otpDec
) where

import Data.Char (ord, chr)
import Data.List (zipWith)

offset :: Int -> Char -> Char
offset k = chr . (+k) . ord

caesarEnc :: Int -> String -> String
caesarEnc k = map (offset k)

caesarDec :: Int -> String -> String
caesarDec = caesarEnc . negate

otpEnc :: [Int] -> String -> String
otpEnc k = zipWith offset (cycle k)

otpDec :: [Int] -> String -> String
otpDec k = otpEnc k'
  where k' = map negate k
