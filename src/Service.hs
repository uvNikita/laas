{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
    main
) where

import qualified Codec.Archive.Tar        as Tar
import           Control.Applicative      ((<$>))
import qualified Data.ByteString.Lazy     as BS
import           Data.Default             (def)
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           boolFlag, optPos, parsedBy,
                                           withParseResult)
import           System.Daemon            (daemonPort, ensureDaemonRunning)
import           System.FilePath          ((-<.>), (</>))
import           System.IO.Error          (tryIOError)
import           System.IO.Temp           (withSystemTempDirectory)

import           System.Process           (callProcess)

import           Types


data Settings = Settings { host      :: String
                         , port      :: Int
                         , daemonize :: Bool } deriving (Show)



argsParser :: ParserSpec Settings
argsParser = Settings
    `parsedBy` optPos   "localhost" "host" `Descr` "Host to to bind to"
    `andBy`    optPos   15000       "port" `Descr` "Port used for service"
    `andBy`    boolFlag "daemonize"        `Descr` "Run program in text mode"


main :: IO()
main = withParseResult argsParser service


service :: Settings -> IO ()
service Settings {..} | daemonize = ensureDaemonRunning "laas" (def { daemonPort = port }) handle
                      | otherwise = error "Not implemented"

handle :: Request -> IO Response
handle (Request { mainName, inputArchive }) = do
    withSystemTempDirectory "laas_contents" $ \ contentsPath -> do
        Tar.unpack contentsPath $ Tar.read inputArchive

        let pdfPath = contentsPath </> mainName -<.> "pdf"
        let mainPath = contentsPath </> mainName

        result <- tryIOError $ callProcess "pdflatex" ["-output-directory=" ++ contentsPath, mainPath]

        case result of
            Right _ -> Ok    <$> BS.readFile pdfPath
            Left  e -> Error <$> return (show e)
