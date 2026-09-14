module Frontend.IOPipeline where

import Frontend.Diagnostics (renderDiagnostic)
import Frontend.Flags
import Frontend.Lexer (scanTokens)
import Frontend.Parser (parse)
import Syntax.Location
import Syntax.Surface
import Syntax.Token
import System.IO

-- run the various stages of the pipeline, printing warnings and errors
-- automatically

ioLex :: (MonadIO m) => FrontendFlags -> FilePath -> String -> m (Maybe [Located Token])
ioLex _ fp str = do
    let lexed = scanTokens str
    case lexed of
        Left err -> putTextLn (renderDiagnostic fp str err) >> return Nothing
        Right (toks, warns) -> do
            forM_ warns (putTextLn . renderDiagnostic fp str)
            return $ Just toks

ioParse :: (MonadIO m) => FrontendFlags -> FilePath -> String -> [Located Token] -> m (Maybe (SurfaceModule Span))
ioParse flags fp str toks = do
    let parsed = parse flags toks
    case parsed of
        Left err -> putTextLn (renderDiagnostic fp str err) >> return Nothing
        Right (modl, warns) -> do
            forM_ warns (putTextLn . renderDiagnostic fp str)
            return $ Just modl

-- once the full frontend is done, this will return the final data for backends, for now () so ghc doesn't yell at us
runFrontend :: FrontendFlags -> FilePath -> IO ()
runFrontend flags fp = do
    contents <- openFile fp ReadMode >>= hGetContents

    -- special case bind for this pseudo-transformer
    let (>>?=) :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
        a >>?= f = do
            a' <- a
            case a' of
                Nothing -> pure Nothing
                Just a'' -> f a''

    void $ ioLex flags fp contents >>?= ioParse flags fp contents
    pure ()
