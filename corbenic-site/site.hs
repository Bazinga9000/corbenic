--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Hakyll
import System.Process.Typed (proc, readProcess)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

config :: IO Configuration
config = do
    dest <- lookupEnv "HAKYLL_DESTINATION"
    pure
        defaultConfiguration
            { destinationDirectory = fromMaybe "_site" dest
            }

--------------------------------------------------------------------------------
-- Compilers
--------------------------------------------------------------------------------

typstCompiler :: Compiler (Item String)
typstCompiler = do
    inputFile <- getResourceFilePath
    (exitCode, out, err) <-
        unsafeCompiler $
            -- the `--features html` flag is required for HTML export
            readProcess (proc "typst" ["compile", "--features", "html", "--format", "html", inputFile, "-"])
    let code = show exitCode
    unless ("ExitSuccess" `elem` words code) $
        fail $
            "typst failed for " ++ inputFile ++ ":\n" ++ show err
    -- typst html exports a full document; extract only <body> and explicitly decode as UTF-8
    let html = TLE.decodeUtf8 out :: TL.Text
        body = fromMaybe html (extractBetween "<body>" "</body>" html)
    makeItem (toString body)

-- extract the text between the first occurrence of open and the next close
extractBetween :: TL.Text -> TL.Text -> TL.Text -> Maybe TL.Text
extractBetween open close s = do
    (_, afterOpen) <- findSub open s
    (beforeClose, _) <- findSub close afterOpen
    pure beforeClose
  where
    findSub pat = go TL.empty
      where
        go acc rest
            | Just r <- TL.stripPrefix pat rest = Just (acc, r)
            | otherwise = case TL.uncons rest of
                Nothing -> Nothing
                Just (c, cs) -> go (TL.snoc acc c) cs

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    setLocaleEncoding utf8
    cfg <- config
    hakyllWith cfg $ do
        match "static/**" $ do
            route (gsubRoute "static/" (const ""))
            compile copyFileCompiler

        match "*.typst" $ do
            route $ setExtension "html"
            compile $
                typstCompiler
                    >>= loadAndApplyTemplate "templates/default.html" siteCtx
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
-- Contexts
--------------------------------------------------------------------------------

siteCtx :: Context String
siteCtx =
    field "title" (\_ -> pure "Corbenic")
        <> defaultContext
