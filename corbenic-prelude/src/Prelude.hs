module Prelude (
    module Relude,
    module Relude.Extra.Bifunctor,
    module Prelude.Pretty,
    askFor,
)
where

import Control.Lens
import Prelude.Pretty
import Relude
import Relude.Extra.Bifunctor

-- lens utility over reader
askFor :: (MonadReader s m) => Getting b s b -> m b
askFor l = (view l) <$> ask
