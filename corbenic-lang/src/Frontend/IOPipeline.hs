module Frontend.IOPipeline where

import Frontend.Diagnostics (Diagnosible, renderDiagnostic)
import Frontend.Flags
import Frontend.Lexer (scanTokens)
import Frontend.Parser (parse)
import Frontend.TypeChecker
import Frontend.TypeChecker.Types
import Syntax.Location
import Syntax.Surface
import Syntax.Token
import System.IO (hGetContents, openFile)

printEither :: (Diagnosible err, Diagnosible warn, MonadIO m) => Either err (a, [warn]) -> FilePath -> String -> m (Maybe a)
printEither e fp str = do
    case e of
        Left err -> putTextLn (renderDiagnostic fp str err) >> return Nothing
        Right (a, warns) -> do
            forM_ warns (putTextLn . renderDiagnostic fp str)
            return $ Just a

-- run the various stages of the pipeline, printing warnings and errors
-- automatically

ioLex :: (MonadIO m) => FrontendFlags -> FilePath -> String -> m (Maybe [Located Token])
ioLex _ fp str = printEither (scanTokens str) fp str

ioParse :: (MonadIO m) => FrontendFlags -> FilePath -> String -> [Located Token] -> m (Maybe (SurfaceModule Span))
ioParse flags fp str toks = printEither (parse flags toks) fp str

ioTypeCheck :: (MonadIO m) => FrontendFlags -> FilePath -> String -> SurfaceModule Span -> m (Maybe (SurfaceModule (Span, CorbenicType)))
ioTypeCheck flags fp str modl = printEither (typeCheckModule flags modl) fp str

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

    void $ ioLex flags fp contents >>?= ioParse flags fp contents >>?= ioTypeCheck flags fp contents
    pass
