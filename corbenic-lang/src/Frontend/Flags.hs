module Frontend.Flags where

import Control.Lens

data FrontendFlags = FrontendFlags
    { _fNoImplicitPrelude :: Bool
    , _fExperimental :: Bool
    , _fWarnError :: Bool
    , _fAllowOrphans :: Bool
    }

defaultFFlags :: FrontendFlags
defaultFFlags =
    FrontendFlags
        { _fNoImplicitPrelude = False
        , _fExperimental = False
        , _fWarnError = False
        , _fAllowOrphans = False
        }

makeLenses ''FrontendFlags
