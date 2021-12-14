module Main where

import ParserNetPbm
import TypesNetPbm

import System.Environment
import System.Directory
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
                          Left err -> print err
                          Right img -> print image
                    else putStrLn "Error: File does not exist"
