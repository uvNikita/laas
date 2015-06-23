{-# LANGUAGE RecordWildCards #-}

module Main (
    main
) where

import qualified Codec.Archive.Tar        as Tar
import           Control.Applicative      ((<$>))
import qualified Data.ByteString.Lazy     as BS
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           optPos, parsedBy, withParseResult)
import           System.Daemon            (runClient)
import           System.FilePath          ((-<.>), (</>))
import           Types

data Settings = Settings { mainName :: String
                         , baseDir  :: String
                         , host     :: String
                         , port     :: Int } deriving (Show)



argsParser :: ParserSpec Settings
argsParser = Settings
    `parsedBy` optPos "main"      "mainName"  `Descr` "Main Latex filename"
    `andBy`    optPos "."         "baseDir"   `Descr` "Latex project directory"
    `andBy`    optPos "localhost" "host"      `Descr` "Hostname of remote LaaS service"
    `andBy`    optPos 15000       "port"      `Descr` "Port of remote LaaS service"

main :: IO ()
main = withParseResult argsParser client


client :: Settings -> IO ()
client (Settings {..}) = do
    tar <- Tar.write <$> Tar.pack "" [baseDir]

    let request = Request { mainName = mainName, inputArchive = tar }
    let pdfPath = baseDir </> mainName -<.> "pdf"
    result <- runClient host port request
    case result of
        Nothing -> print "Connection closed"
        Just r  -> case r of
                       Ok    pdf -> BS.writeFile pdfPath pdf
                       Error msg -> print $ "Error occured: " ++ msg
