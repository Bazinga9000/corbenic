--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           System.Environment (lookupEnv)
import           System.Process.Typed (proc, readProcess)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Control.Monad (unless)
import           Data.List (isPrefixOf)
import           Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

config :: IO Configuration
config = do
    dest <- lookupEnv "HAKYLL_DESTINATION"
    pure defaultConfiguration
        { destinationDirectory = maybe "_site" id dest
        }

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------

typstCompiler :: Compiler (Item String)
typstCompiler = do
    inputFile <- getResourceFilePath
    (exitCode, out, err) <- unsafeCompiler $
        -- the `--features html` flag is required for HTML export
        readProcess (proc "typst" ["compile", "--features", "html", "--format", "html", inputFile, "-"])
    let code = show exitCode
    unless ("ExitSuccess" `elem` words code) $
        fail $ "typst failed for " ++ inputFile ++ ":\n" ++ LBS.unpack err
    -- typst html exports a full document; extract only <body>
    let html = LBS.unpack out
        body = fromMaybe html (extractBetween "<body>" "</body>" html)
    makeItem body

-- extract the text between the first occurrence of open and the next close
extractBetween :: String -> String -> String -> Maybe String
extractBetween open close s = do
    (_, afterOpen) <- findSub open s
    (beforeClose, _) <- findSub close afterOpen
    pure beforeClose
  where
    findSub pat = go []
      where
        go _ [] = Nothing
        go acc rest@(c:cs)
            | pat `isPrefixOf` rest = Just (reverse acc, drop (length pat) rest)
            | otherwise = go (c:acc) cs

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = config >>= \cfg -> hakyllWith cfg $ do
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "*.typst" $ do
        route $ setExtension "html"
        compile $ typstCompiler
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    field "title" (\_ -> pure "Corbenic") <>
    defaultContext
