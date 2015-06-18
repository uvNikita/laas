{-# LANGUAGE RecordWildCards #-}
module Main (
    main
) where

import qualified Codec.Archive.Tar        as Tar
import qualified Data.ByteString          as BS
import           Data.Default             (def)
import           System.Console.ArgParser (Descr (..), ParserSpec, andBy,
                                           boolFlag, optPos, parsedBy,
                                           withParseResult)
import           System.Daemon            (daemonPort, ensureDaemonRunning)
import           System.Directory         (getTemporaryDirectory)
import           System.IO                (hClose)
import           System.IO.Temp           (createTempDirectory,
                                           openBinaryTempFile)
import System.Process (callProcess)
import System.FilePath ((<.>), (</>))
import Control.Applicative ((<$>), (<*>), pure)

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
handle (Request {..}) = do
    tmp <- getTemporaryDirectory
    (tarPath, hTar) <- openBinaryTempFile tmp "laas.tar"
    BS.hPut hTar inputArchive
    hClose hTar

    contentsPath <- createTempDirectory tmp "laas_contents"
    Tar.extract contentsPath tarPath

    callProcess "pdflatex" [mainName]

    let pdfPath = contentsPath </> mainName <.> "pdf"

    Response <$> BS.readFile pdfPath
