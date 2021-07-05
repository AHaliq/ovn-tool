module Main where

import Lib
import Options.Applicative
import Data.Semigroup ((<>))

version = "0.1.0.0"
data Input
  = Version
  | GenKeys
  | NIZK String -- path to privatekey file
  | CDS String String -- path to privatekey file, voter data file

versionParser :: Parser Input
versionParser = flag' Version
  ( long "version"
  <> short 'v'
  <> help "Get program version number" )

keysParser :: Parser Input
keysParser = flag' GenKeys
  ( long "keys"
  <> short 'k'
  <> help "Generate private and public key files " )

skParser :: Parser String
skParser = strOption
  ( long "secret"
  <> short 's'
  <> metavar "PATH-TO-PRIVATE-KEY-FILE"
  <> help "Path to private key file" )

voterParser :: Parser String
voterParser = strOption
  ( long "data"
  <> short 'd'
  <> metavar "PATH-TO-VOTER-DATA-FILE"
  <> help "Path to voter data file" )

nizkParser :: Parser Input
nizkParser = 
  flag' id
  ( long "nizk"
  <> short 'n'
  <> help "Generate NIZK proof file" ) <*>
  (NIZK <$> skParser)

cdsParser :: Parser Input
cdsParser =
  flag' id
  ( long "cds"
  <> short 'c'
  <> help "Generate CDS proof file" ) <*> (
  CDS <$> skParser <*> voterParser)

fullParser :: Parser Input
fullParser = versionParser <|> keysParser <|> nizkParser <|> cdsParser


main :: IO ()
main = program =<< execParser opts
  where
    opts = info (fullParser <**> helper)
      ( fullDesc
      <> progDesc "ovn tool for generating secrets "
      <> header "ovn-tool" )

program :: Input -> IO ()
program Version = putStrLn $ "ovn-tool v" ++ version
program GenKeys = putStrLn "successfully generated pk and sk"
program (NIZK pk) = putStrLn "successfully generated NIZK proof"
program (CDS pk vd) = putStrLn "successfully generated CDS proof"
