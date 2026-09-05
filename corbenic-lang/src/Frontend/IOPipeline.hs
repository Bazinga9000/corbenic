module Frontend.IOPipeline where

import Frontend.Diagnostics (renderDiagnostic)
import Frontend.Lexer
import Syntax.Location
import Syntax.Token

-- run the various stages of the pipeline, printing warnings and errors
-- automatically

ioLex :: (MonadIO m) => FilePath -> String -> m (Maybe [Located Token])
ioLex fp str = do
    let lexed = scanTokens str
    case lexed of
        Left err -> putTextLn (renderDiagnostic fp str err) >> return Nothing
        Right (toks, warns) -> do
            forM_ warns (putTextLn . renderDiagnostic fp str)
            return $ Just toks
