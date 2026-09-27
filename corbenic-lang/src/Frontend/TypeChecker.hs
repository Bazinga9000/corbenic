module Frontend.TypeChecker where

import Control.Monad.Except
import Control.Monad.RWS hiding (pass)
import Data.Set qualified as S
import Frontend.Flags
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Infer
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface

-- the core type checker is divided into several strata, so that we can collectively gain
-- more information and handle mutual recursion properly

-- strata 1: term headers
data TermHeader = TermHeader
    { thVis :: Visibility
    , thName :: Identifier
    , thSig :: Maybe (SurfaceTypeDecl Span)
    , thDecl :: SurfaceTermDecl Span
    }

data Visibility = Visible Span | Invisible Span

collectTermHeaders :: [MaybeExported SurfaceDeclaration Span] -> [TermHeader]
collectTermHeaders = catMaybes . map collect
  where
    collect (Exported sp (SDTerm mSig td)) = Just $ header (Visible sp) mSig td
    collect (Hidden sp (SDTerm mSig td)) = Just $ header (Invisible sp) mSig td
    collect _ = Nothing

    header vis mSig td@(SurfaceTermDecl _ _ (Annotated _ name) _) =
        TermHeader{thVis = vis, thName = name, thSig = mSig, thDecl = td}

checkDupes :: [TermHeader] -> Tc ()
checkDupes = go mempty
  where
    go _ [] = pass
    go seen (TermHeader _ name _ td : hs)
        | S.member name seen = throwError $ TypeCheckerError (spanOf td) $ TCDuplicateDeclaration name
        | otherwise = go (S.insert name seen) hs

checkTerms :: [TermHeader] -> Tc [MaybeExported SurfaceDeclaration (Span, CorbenicType)]
checkTerms termHeaders = do
    let binds = [(thName h, stydType <$> thSig h, stdBody $ thDecl h) | h <- termHeaders]
    checkKnot binds $ \finalized -> return $ zipWith rebuild termHeaders finalized
  where
    rebuild h (_, _, body') =
        let bt = exprType body'
            SurfaceTermDecl sp doc (Annotated spi name) _ = thDecl h
            td' = SurfaceTermDecl (sp, bt) doc (Annotated (spi, bt) name) body'
            d' = SDTerm (thSig h) td'
         in case thVis h of
                Visible sp' -> Exported sp' d'
                Invisible sp' -> Hidden sp' d'


checkModule :: SurfaceModule Span -> Tc (SurfaceModule (Span, CorbenicType))
checkModule modl = do
    let termHeaders = collectTermHeaders . smDecls $ modl
    checkDupes termHeaders
    decls' <- checkTerms termHeaders
    return modl { smDecls = decls' }


initialTcEnv :: FrontendFlags -> TcEnv
initialTcEnv fflags = TcEnv
    { _tcTerms = mempty
    , _tcTyCons = mempty
    , _tcClasses = mempty
    , _tcInstances = mempty
    , _tcAliases = mempty
    , _tcConstraintAliases = mempty
    , _tcTypeVars = mempty
    , _flags = fflags
    }

initialTcState :: TcState
initialTcState = TcState { _metaN = 0, _rigidN = 0, _kindN = 0, _currentSubst = mempty }

runTc :: FrontendFlags -> Tc a -> Either TypeCheckerError (a, [TypeCheckerWarning])
runTc fflags m =
    case runExcept (runRWST m (initialTcEnv fflags) initialTcState) of
        Left err -> Left err
        Right (a, _st, w) -> Right (a, twWarnings w)


typeCheckModule :: FrontendFlags -> SurfaceModule Span -> Either TypeCheckerError (SurfaceModule (Span, CorbenicType), [TypeCheckerWarning])
typeCheckModule fflags modl = runTc fflags (checkModule modl)
