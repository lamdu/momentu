module Control.Monad.Reader.Extended
    ( module Control.Monad.Reader
    , pushToReader, pushToReaderExt
    ) where

import           Control.Monad.Reader

import           Prelude

-- | With the reader monad,
-- whether a function parameter is inside the monadic action's result,
-- or whether it was given outside of it, is equivalent!
--
-- It is usually more ergonomic for the parameter to be outside,
-- but in some case one needs to move a parameter inside.
--
-- This is the transformation that @pushToReader@ performs.
-- Its opposite is the @Control.Lens.(??)@ operator.
pushToReader :: MonadReader env m => (a -> env -> r) -> m (a -> r)
pushToReader = asks . flip

-- | Extend @pushToReader@ for more arguments.
--
-- 'pushToReaderExt pushToReader :: MonadReader env m => (a -> b -> env -> r) -> m (a -> b -> r)'
pushToReaderExt :: MonadReader env m => (b -> env -> r) -> (a -> b) -> m (a -> r)
pushToReaderExt f = pushToReader . fmap f
