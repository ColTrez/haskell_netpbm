module Main where

import ParserNetPbm
import TypesNetPbm
import ConvertNetPbm

import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString as B

main :: IO ()
main = do
    args <- getArgs
    parseCommand args
                               

filePath :: FilePath
filePath = "./testImages/smileAscii.pbm"

parseCommand :: [String] -> IO ()
parseCommand (com:args)
    | com == "help" = help args
    | com == "printInfo" = printInfo args
    | com == "convert" = convert args
    | otherwise = putStrLn "Command not recognized"

notRecognized :: String -> IO ()
notRecognized com = putStrLn $ "The command " ++ com ++  " was not recognized. Run with argument help to print list of recognized commands"

help :: [String] -> IO ()
help args = putStrLn "Recognized commands are help"

printInfo :: [String] -> IO ()
printInfo [] = putStrLn "Error: No filepath provided. Please include relative path to file."
printInfo args = let filename = head args
                 in do
                    exists <- doesFileExist filename
                    if exists then do
                        file <- B.readFile filename
                        let image = parseImage file
                        case image of
                          Left err -> putStrLn err
                          Right img -> print image
                    else putStrLn "Error: File does not exist"

convert :: [String] -> IO ()
convert [] = putStrLn "Error: No filepath provided. Please include relative path to file."
convert (fn:[]) = putStrLn "Error: No filetype specified. Please include filetype after filename"
convert (fn:filetype:[]) = putStrLn "Error: RAW or ASCII must be specified"
convert (fn:filetype:mode:xs) =
    do
    exists <- doesFileExist fn
    if exists then do
        file <- B.readFile fn
        let image = parseImage file
            ft = parseFileTypeArg filetype
            fm = parseFileModeArg mode
            convertAttempt = liftM3 convertImage image ft fm
        case convertAttempt of
          Left err -> putStrLn err
          Right ioAction -> ioAction
    else putStrLn "Error: File does not exist"

