module Frontend.TypeChecker where

import Control.Monad.Except
import Control.Monad.RWS hiding (pass)
import Control.Lens
import Data.Set qualified as S
import Data.Map qualified as M
import Frontend.Flags
import Frontend.TypeChecker.Error
import Frontend.TypeChecker.Infer
import Frontend.TypeChecker.Realize
import Frontend.TypeChecker.Tc
import Frontend.TypeChecker.Types
import Syntax.Identifier
import Syntax.Location
import Syntax.Surface

-- the core type checker is divided into several strata, so that we can collectively gain
-- more information and handle mutual recursion properly

-- strata 2: data headers
data DataHeader = DataHeader
    { dhVis    :: Visibility
    , dhName   :: Annotated Span Identifier
    , dhParams :: [Annotated Span Identifier]
    , dhCtors  :: [MaybeExported SurfaceTypeConstructor Span]
    , dhIsNewtype :: Bool
    }

instance HasSpan DataHeader where
    spanOf = spanOf . dhName
instance Named DataHeader where
    getName = getName . dhName


collectDataHeaders :: [MaybeExported SurfaceDeclaration Span] -> [DataHeader]
collectDataHeaders = catMaybes . map collect
  where
    collect (Exported sp (SDData sdd)) = Just $ headerD (Visible sp) sdd
    collect (Hidden sp (SDData sdd)) = Just $ headerD (Invisible sp) sdd
    collect (Exported sp (SDNewtype sntd)) = Just $ headerNT (Visible sp) sntd
    collect (Hidden sp (SDNewtype sntd)) = Just $ headerNT (Invisible sp) sntd
    collect _ = Nothing

    headerD vis (SurfaceDataDecl _ _ name params ctors) =
        DataHeader{dhVis = vis, dhName = name, dhParams = params, dhCtors = ctors, dhIsNewtype = False}
    headerNT vis (SurfaceNewtypeDecl _ _ name params ctor) =
        DataHeader{dhVis = vis, dhName = name, dhParams = params, dhCtors = one ctor, dhIsNewtype = True}

-- for each data type declaration, infer a fresh kind variable for each of its parameters
-- and assemble the final kind of the type with those variables
makeDataType :: DataHeader -> Tc (Identifier, CorbenicKind, [(Identifier, TypeVar)])
makeDataType h = do
    checkDupes $ dhParams h
    checkDupes $ dhCtors h
    ps <- forM (dhParams h) $ \case
        (Annotated sp ident) -> do
            tv <- freshKind >>= freshMetavarTV sp
            return (ident, tv)
    let k = foldr CKArr CKStar (map (tvKind . snd) ps)
    return $ (annVal (dhName h), k, ps)

-- given the fresh tvars of the type's parameters, infer a type for a specific constructor
makeCtor :: DataHeader -> [(Identifier, TypeVar)] -> SurfaceTypeConstructor Span -> Tc (Identifier, Scheme)
makeCtor h ps (SurfaceTypeConstructor _ _ (Annotated sp cn) fields) = do
    fieldTypes <- mapM realize fields
    let resultType = foldl' applyOne ctorType (map snd ps) where
        ctorType = CTCon sp (annVal (dhName h))
        applyOne t tv = CTApp sp t (CTVar tv)
    let body = foldr (mkFun sp) resultType fieldTypes
    return (annVal (Annotated sp cn), Scheme (map snd ps) [] body)

checkData :: [DataHeader] -> Tc a -> Tc a
checkData hds cont = do
    outs <- mapM makeDataType hds

    -- machinery for getting the relevant param variables
    let paramVars = M.fromList $ map (\(a,b,c) -> (a,(b,c))) outs
    let getParamVars dh = fromMaybe [] $ snd <$> M.lookup (annVal (dhName dh)) paramVars
    let getParamVarsAsMap = M.fromList . getParamVars
    let withParamVars dh = over tcTypeVars (M.union (getParamVarsAsMap dh))

    let kMap = map (\(ident, k, _) -> (ident, k)) outs
    local (over tcTyCons (M.union (fromList kMap))) $ do
        let mkOne dh = local (withParamVars dh) $
                mapM (\ctor -> makeCtor dh (getParamVars dh) (unMaybeExported ctor)) (dhCtors dh)
        ctorTerms <- join <$> mapM mkOne hds
        local (over tcTerms (M.union (fromList ctorTerms))) cont


-- strata 1: term headers
data TermHeader = TermHeader
    { thVis :: Visibility
    , thName :: Identifier
    , thSig :: Maybe (SurfaceTypeDecl Span)
    , thDecl :: SurfaceTermDecl Span
    }

instance HasSpan TermHeader where
    spanOf = spanOf . thDecl
instance Named TermHeader where
    getName = thName

data Visibility = Visible Span | Invisible Span

collectTermHeaders :: [MaybeExported SurfaceDeclaration Span] -> [TermHeader]
collectTermHeaders = catMaybes . map collect
  where
    collect (Exported sp (SDTerm mSig td)) = Just $ header (Visible sp) mSig td
    collect (Hidden sp (SDTerm mSig td)) = Just $ header (Invisible sp) mSig td
    collect _ = Nothing

    header vis mSig td@(SurfaceTermDecl _ _ (Annotated _ name) _) =
        TermHeader{thVis = vis, thName = name, thSig = mSig, thDecl = td}

checkDupes :: (HasSpan a, Named a) => [a] -> Tc ()
checkDupes = go mempty
  where
    go _ [] = pass
    go seen (h : hs)
        | S.member (getName h) seen = throwError $ TypeCheckerError (spanOf h) $ TCDuplicateDeclaration (getName h)
        | otherwise = go (S.insert (getName h) seen) hs

checkNoPrimitives :: (HasSpan a, Named a) => [a] -> Tc ()
checkNoPrimitives = mapM_ $ \x -> case getName x of
    IdentPrimitive prim -> throwError $ TypeCheckerError (spanOf x) $ TCDeclaredPrimitive prim
    _ -> pass

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
    let dataHeaders = collectDataHeaders . smDecls $ modl
    let allHeaders = map SomeLocatedName termHeaders <> map SomeLocatedName dataHeaders
    checkDupes allHeaders
    checkNoPrimitives allHeaders
    decls' <- checkData dataHeaders $ checkTerms termHeaders
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
initialTcState = TcState { _metaN = 0, _rigidN = 0, _kindN = 0, _currentSubst = mempty, _kindSubst = mempty }

runTc :: FrontendFlags -> Tc a -> Either TypeCheckerError (a, [TypeCheckerWarning])
runTc fflags m =
    case runExcept (runRWST m (initialTcEnv fflags) initialTcState) of
        Left err -> Left err
        Right (a, _st, w) -> Right (a, twWarnings w)


typeCheckModule :: FrontendFlags -> SurfaceModule Span -> Either TypeCheckerError (SurfaceModule (Span, CorbenicType), [TypeCheckerWarning])
typeCheckModule fflags modl = runTc fflags (checkModule modl)
