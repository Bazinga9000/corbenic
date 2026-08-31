module Main where

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
            Just input -> do
                putStrLn $ "You input: " <> input
                loop
