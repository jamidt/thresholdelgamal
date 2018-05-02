module Main where

import Lib
import System.Random (randomR, newStdGen, getStdGen)

genSafePrimePair :: Integer -> SafePrimePair
genSafePrimePair q = (SafePrimePair p q)
                    where p = 2 * q + 1

main :: IO ()
main = do
    let p = 359
    let safePair = genSafePrimePair p

    putStrLn "Classical ElGamal encryption and decryption"

    putStrLn $ show safePair
    gen <- getStdGen

    let (privKey, pubKey, gen') = generateKeys gen safePair

    putStrLn $ show privKey
    putStrLn $ show pubKey

    let (k, gen'') = randomR (0, p - 1) gen'
    let m = Message k

    putStrLn $ show m

    let (cyph, gen''') = encrypt gen'' pubKey m

    putStrLn $ show cyph

    let dec = decrypt pubKey privKey cyph
    putStrLn $ show dec

