module LynCrypt.Util where

slice a b xs = (take (b'-a'+1)) $ (drop a' xs)
    where a' = fromIntegral a :: Int
          b' = fromIntegral b :: Int
