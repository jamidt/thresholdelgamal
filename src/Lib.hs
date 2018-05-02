{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns  #-}

module Lib( PrivateKey,
            PublicKey,
            SafePrimePair(..),
            Cypher,
            Message(..),
            encrypt,
            decrypt,
            generateKeys,
            ThresholdPair,
            Share,
            calcShares,
            calcShared,
            decryptShared
    ) where

import System.Random
import GHC.Integer.GMP.Internals (powModInteger)
import Utils
import Data.Ratio

-- | Private key
--
-- First entry is prime p and random element a of F_q=Z/qZ
data PrivateKey = PrivateKey{
    s :: Integer
} deriving (Show)

-- |Public key
--
-- First entry is prime p = 2q + 1, with q prime, random generator g of Fq and the
-- third is the result of g^a mod q
data PublicKey = PublicKey{
    p :: Integer,
    g :: Integer,
    b :: Integer
} deriving (Show)

-- |Cypher text
data Cypher = Cypher{
    c1 :: Integer,
    c2 :: Integer
} deriving (Show)

-- | Message
data Message = Message{
    message :: Integer
} deriving (Show)

-- | Safe prime
--
-- Prime p, such that p = 2q + 1 and q being a prime, too
data SafePrimePair = SafePrimePair{
    bigPrime :: Integer,
    subPrime :: Integer
} deriving (Show)

-- | Message encryption
--
-- Takes a random generator, to generate k in F_q and compute
-- c_1 = g^k mod p
-- c_2 = m b^k mod p
encrypt :: RandomGen g => g -> PublicKey -> Message -> (Cypher, g)
encrypt gen PublicKey{..} Message{..} = 
    let (k, gen') = randomR (0, p-1) gen
        c1 = powModInteger g k p
        c2 = (message * powModInteger b k p) `mod` p
    in (Cypher c1 c2, gen')

-- | Decrypt message
--
decrypt :: PublicKey -> PrivateKey -> Cypher -> Message
decrypt PublicKey{..} PrivateKey{..} Cypher{..} = 
    let x = p - 1 - s
    in Message $ (c2 * powModInteger c1 x p) `mod` p

-- | Generate private and public key
--
generateKeys :: RandomGen g => g -> SafePrimePair -> (PrivateKey, PublicKey, g)
generateKeys gen sp@SafePrimePair{bigPrime = p, subPrime = q} = 
    let (privateExponent, gen') = randomR (1, q - 1) gen
        (generator, gen'') = randGenerator gen' sp
        publicExpGenerator = powModInteger generator privateExponent p
    in (PrivateKey privateExponent, PublicKey p generator publicExpGenerator, gen'')

-- | Get generator of the cyclic group Fp
randGenerator :: RandomGen g => g -> SafePrimePair -> (Integer, g)
randGenerator gen sp@SafePrimePair{bigPrime = p, subPrime = q} =
    let (k, gen') = randomR (2, p - 1) gen
        generator = powModInteger k 2 p
    in if generator /= 1 then (generator, gen') else randGenerator gen' sp

-- | ThresholdPair
--
-- (k, n) threshold pair, with n the number of shares, k the required
-- number of shares to reconstruct the secret
data ThresholdPair = ThresholdPair{
    k :: Integer,
    n :: Integer
} deriving (Show)

createPolynomial :: RandomGen g => g -> PublicKey -> PrivateKey -> Int -> (Polynomial, g)
createPolynomial gen PublicKey{..} PrivateKey{..} k =
                        let (coeff, gen') = randomList (0, p-1) (k - 1) gen
                            coeff' = s : coeff
                        in (Polynomial coeff p, gen')

data Share = Share{
    sharex :: Integer,
    sharey :: Integer
} deriving (Show)

-- | Calculate n shares
--
-- todo: randomize x_i
calcShares :: Polynomial -> Integer -> [Share]
calcShares poly n = [Share x $ f x | x <- [1..n]]
            where f = eval poly

calcShared :: PublicKey -> [Share] -> Cypher -> Integer
calcShared PublicKey{..} shares Cypher{..} =
                    let xVal = map sharex shares
                        lagrangeInterPoints = lagrangeInterpolation p xVal
                        yVal = map sharey shares
                        exponent = sum $ zipWith (*) lagrangeInterPoints yVal
                    in powModInteger c1 exponent p

decryptShared :: PublicKey -> Integer -> Cypher -> Integer
decryptShared PublicKey{..} sh Cypher{..} = c2 * powModInteger sh (p - 2) p `mod` p
