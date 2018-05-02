{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Utils (randomList,
              Polynomial(..),
              eval,
              lagrangeInterpolation
             ) where

import System.Random
import GHC.Integer.GMP.Internals (powModInteger)

-- | Generate list of random numbers
--
-- Returns a tuple of a list of random numbers and the last state of the
-- random number generator
randomList :: (RandomGen g, Random a) => (a, a) -> Int -> g -> ([a], g)
randomList range n =  selR . take n . iterate (\x -> randomR range $ snd x) . randomR range
    where selR x = (map fst x, snd $ last x)

data Polynomial = Polynomial{
    coeffList :: [Integer],
    modulo :: Integer
} deriving (Show)

eval :: Polynomial -> Integer -> Integer
eval Polynomial{..} x = sum [ a * x^n | (a, n) <- zip coeffList [0..] ] `mod` modulo

lagrangeInterpolation :: Integer -> [Integer] -> [Integer]
lagrangeInterpolation p l =
                        let nom i = product $ filter (/=i) l
                            denom i = product $ (\x -> (x - i)) <$> filter (/=i) l
                            factor i = nom i * powModInteger (denom i) (p - 2) p
                        in (\x -> factor x `mod` p) <$> l
