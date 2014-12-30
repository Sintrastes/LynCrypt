{-# LANGUAGE ViewPatterns #-}

module Main where

import LynCrypt
import System.Console.ArgParser
import System.Environment
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Serialize
import qualified Data.ByteString as B
import System.Random

help = ["|-------- LynCrypt 0.1.0.0, help -------|",
        "|",
        "| LynCrypt is both a haskell library and an executable for using the",
        "| Asmuth-Bloom secret sharing scheme. This is the full documentation",
        "| for the executable, and the library documentation can be found in the",
        "| included Hackage docs. Before using, please see lyncrypt --info for.",
        "| important information regarding the encryption scheme itself and how to",
        "| use it.",
        "|",
        "| Usage:",
        "|    lyncrypt --help",
        "|        Display this help sceen.",
        "|    lyncrypt --encrypt file",
        "|        Uses file as input for encryption.",
        "|    lyncrypt --decrypt m0_file share1_file share2_file...",
        "|        Choose the files to use as input for decryption",
        "|    lyncrypt --stdout",
        "|        Use stdout as the output decice",
        "|    lyncrypt --output file",
        "|        Use file as the output device",
        "|    lyncrypt -k x",
        "|        Sets the threshold value \"k\" to x. \"k\" should be less than than n. ",
        "|    lyncrypt -n x",
        "|        Sets the total number of shares generated \"n\" to x.",
        "|    lyncrypt --readable",
        "|        Use a readable serialized output instead of the default binary output."]

info = ["|-------- LynCrypt 0.1.0.0, info -------|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|",
        "|"]

data Data = Data Int Int String [String] [String] Bool 
  deriving (Eq, Show)

cmdParser :: ParserSpec Data
cmdParser = Data 
     `parsedBy` reqPos   "n"        
     `andBy`    reqPos   "k"        
     -- Use the null character to represent "nothing", since argparser doesn't support
     -- Maybe values, and the null character is an invalid filename.
     `andBy`    optFlag    "\0" "encrypt"                 
     `andBy`    optFlagArgs  [] "decrypt"  [] (\y x -> y++[x])
     `andBy`    optFlagArgs  [] "output"   [] (\y x -> y++[x])
     `andBy`    boolFlag        "readable"


cmdInterface :: IO (CmdLnInterface Data)
cmdInterface =
  (`setAppDescr` "top description")
  <$> (`setAppEpilog` "bottom description")
  <$> mkApp cmdParser

stringDecode :: String -> [Integer]
stringDecode x = map (fromIntegral . fromEnum) x

stringEncode :: [Integer] -> String
stringEncode x = map (toEnum.fromIntegral) x

-- TODO: Factor this into something more readable
main = do
  rgen <- newStdGen
  args <- getArgs
  interface <- cmdInterface
  let parsed = parseArgs args interface
  case parsed of
    -- Use my custom help screen.
    Left x -> mapM_ putStrLn help
    -- Bind successfuly parsed arguments
    Right (Data n k encrypt
      (d:ds) (o:os)
      readable) -> 
         if encrypt == "\0" && (d:ds) == []
           then putStrLn "Must specify an input file for either encryption, or decryption. See lyncrypt --help for proper usage."
         else do 
           -- *** Encrypt *** --
           if encrypt /= "\0"
             then do 
               message <- (liftM stringDecode $ readFile encrypt)
               let (shares,m0s) = makeShares message (fromIntegral n) (fromIntegral k) rgen
               if readable
                 then do
                   Prelude.writeFile o (show m0s) 
                   -- Write each one of the shares to a seperate output file
                   zipWithM_ Prelude.writeFile os (map show shares)
                else do
                   -- Write m0 to the first specified file)
                   B.writeFile o (encode m0s) 
                   -- Write each one of the shares to a seperate output file
                   zipWithM_ B.writeFile os (map encode shares)

            -- *** Decrypt *** --
            else do
               -- TODO: Better error handling
               m0s <- B.readFile d
               shares <- mapM B.readFile ds
               

               -- need to figure out a better format for this, but it works for now.
               -- I could use optflag args.
               parse_expr <- return ((map decode shares :: [Either String [(Integer,Integer)]]),
                                     (decode m0s :: Either String [Integer]))
               case parse_expr of
                   (as@(anyRight -> True),
                    Right b) -> Prelude.writeFile o $ stringEncode $ decryptShares (fromEither as) b
                   -- TODO: Make error more specific
                   _         -> print "Error deserializing files"

anyRight xs = foldr1 (||) $ map isRight xs
    where isRight (Right _) = True
          isRight _         = False

fromEither [] = []
fromEither (Right x:xs) = x : (fromEither xs) 

