module LynCrypt where

import System.Random
import Data.Char
import System.IO
import Math.NumberTheory.GCD (extendedGCD)
import Math.NumberTheory.Moduli (invertMod)
import Math.NumberTheory.Primes (primes)
import Math.NumberTheory.Prime (nextPrime)
import Data.Serialize
import Control.Monad
import Data.List

slice a b xs = (take (b'-a'+1)) $ (drop a' xs)
    where a' = fromIntegral a :: Int
          b' = fromIntegral b :: Int

chineseRemainder :: [(Integer,Integer)] -> Integer
-- ^ Takes a system of linear congruences x = s (mod m), passed as a list of 
-- tuples (s,m), and solves for it's solution x using the chinese remainder theorem.
chineseRemainder xs = go xs (product $ map snd xs)
    where go x@((a,b):rest) m
            | Nothing <- invertMod (m`div`b) b 
              = error "number and modulus not coprime"
            | Just y <- invertMod (m`div`b) b  
              = (a*(m`div`b)*y + go rest m) `mod` m
          go [] m = 0

euclidGCD a 0 = a 
euclidGCD a b = euclidGCD b (a `mod` b)

-- TODO: Is this property always satisfied for an arbitrary sequence of primes?
-- | Proposition for the precodnition of the Asmuth-Bloom sequence
m_prop :: [Integer] -> Integer -> Bool
m_prop m@(m0:ms) k = m0*(product (slice (n-k+(2::Integer)) n m)) < product (slice (1::Integer) k m)
    where n = toInteger (length ms) :: Integer

--m_prop_dbg m@(m0:ms) k = ((m0:(slice (n-k+2) n m), m0*(product (slice (n-k+2) n m))), 
--                          (slice 1 k m), product (slice 1 k m))
--    where n = toInteger (length ms) :: Integer

asmuthBloom :: Integer -> Integer -> Integer -> [Integer]
asmuthBloom k n s | seq  <- generateFromS (n+1) s
                  , True <- m_prop seq k
                  = seq
                  | seq  <- generateFromS (n+1) s
                  , False <- m_prop seq k
                  = widen seq k

    where generateFromS 0 s = []
          generateFromS n s = [nextPrime s] ++ generateFromS (n-1) (nextPrime s)

-- Widen the gap between m0 and the rest of the sequence until the
-- sequence satisfies the asmuth bloom property
-- May want to use a different technique later, but this should work for now.
widen :: [Integer] -> Integer -> [Integer]
widen seq k | m_prop seq' k = seq'
            | otherwise     = widen seq' k 
   where seq' = deleteAt 1 $ seq++[nextPrime (last seq)]

-- deleteAt :: [a] -> [a]
deleteAt n [] = []
deleteAt n xs = let (h, t) = splitAt n xs in h ++ tail t

widen_dbg seq = deleteAt 1 $ seq++[nextPrime (last seq)]

-- | Choose a random alpha such that s + alpha*m0 < m1..mk
genAlpha :: (RandomGen g) => [Integer] -> Integer -> Integer -> g -> (Integer,g)
genAlpha m@(m0:ms) s k rgen = randomR (1,range) rgen
    where range = ((product (slice (1::Integer) k ms)) - s)`div`m0 -- range should be 0-this number


generateShares :: Integer -> Integer -> [Integer] -> [Integer]
-- ^ Given an s, alpha, and list of ms (including m0), generates the part of the share
-- for a single integer.
generateShares s alpha (m0:ms) = shares
    where xs = replicate (length ms) (s+alpha*m0)
          shares = zipWith mod xs ms

-- Pure version of makeShares
makeShares :: (RandomGen g) => [Integer] -> Integer -> Integer -> g -> ([[(Integer,Integer)]],[Integer])
makeShares xs@(x:_) n k rgen = let shares = transpose $ map fst $ mapWithg (makeShare) rgen xs
                                   m0s    = map snd $ mapWithg makeShare rgen xs
                               in (shares,m0s)
    where makeShare x rgen = let ms = asmuthBloom k n x
                                 m0 = head $ ms
                                 (alpha,r) = genAlpha ms x k rgen
                                 s = generateShares x alpha ms
                                 share =  zip s (tail ms)
                                 in ((share,m0),r)

          mapWithg :: (RandomGen g) => (a -> g -> (b,g)) -> g -> [a] -> [b]
          mapWithg f g [] = [] 
          mapWithg f g (x:xs) = result : mapWithg f next_g xs
              where (result,next_g) = f x g

makeShares' :: [Integer] -> Integer -> Integer -> IO ([[(Integer,Integer)]],[Integer])
makeShares' xs n k = do
    rgen <- newStdGen
    return $ makeShares xs n k rgen

decryptShares :: [[(Integer,Integer)]] -> [Integer] -> [Integer]
-- ^ Takes a list of shares, a list of m0s, and returns an encoded list of integers if the
-- decryption was successful.
-- Note that shares is assumed to be a list of length k, each containing lists of length
-- n, where k is the number of shares, and n is the size of the message. This is the same
-- format as makeShares.
decryptShares shares m0s = zipWith (\share m0 -> (chineseRemainder share) `mod` m0) (transpose shares) m0s

