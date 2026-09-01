module Main where

import Frontend.IOPipeline
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        let corbText = "corbenic> "
        minput <- getInputLine corbText
        case minput of
            Nothing -> pass
            Just ":q" -> pass
            Just (':' : 'l' : ' ' : toLex) -> do
                mToks <- ioLex "<stdin>" toLex
                case mToks of
                    Nothing -> loop
                    Just toks -> putTextLn (prettyPrint toks) >> loop
            Just (':' : rest) -> do
                -- todo color
                putStrLn $ "Invalid REPL command :" <> rest
            Just input -> do
                putStrLn $ "You input: " <> input
                loop
