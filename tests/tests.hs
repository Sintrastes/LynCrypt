module Main where

import System.Exit (exitFailure)
import Test.QuickCheck
import LynCrypt
{-
tests = [
    -- Test the chinese remainder theorem
    \xs -> map (\(a,b) -> (chineseRemainder a b) == a `mod` b),

    -- Test to see if the decryption function is an inverse
    -- of the encryption function with all k
    \xs n k -> do 
        (shares,m0s) <- makeShares xs n k 
        decrypted <- decryptShares shares m0s
        return $ xs == decrypted,

    -- Test to see if the decryption function is an inverse
    -- of the encryption function iff there are k or more shares.]
-}
main = do
    --mapM_ quickCheck tests
    exitFailure
