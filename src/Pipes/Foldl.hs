{-| Each one of these folds is a drop-in replacement for an existing fold from
    the @pipes@ ecosystem, modified to accept 'Fold's or 'FoldM's from the
    @foldl@ library.  These folds are grouped by the module the original fold
    came from
-}

module Pipes.Foldl (
    -- * Pipes.Prelude
      fold
    , foldM

    -- * Pipes.Parse
    , foldAll
    , foldAllM
    , folds
    , foldsM

    -- * Pipes.ByteString
    , foldBytes

    -- * Re-exports
    -- $reexports
    , module Control.Foldl
    , module Control.Monad.Trans.Free
    , module Pipes
    , module Pipes.ByteString
    , module Pipes.Parse
    ) where

import Control.Foldl (Fold, FoldM)
import qualified Control.Foldl as L
import Control.Monad.Trans.Free (FreeT)
import Pipes (Producer)
import Pipes.ByteString (Word8, ByteString)
import qualified Pipes.ByteString as PB
import Pipes.Parse (Parser)
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as P

-- | Strict fold of the elements of a 'Producer'
fold :: (Monad m) => Fold a b -> Producer a m () -> m b
fold (L.Fold step begin done) = P.fold step begin done
{-# INLINABLE fold #-}

-- | Strict, monadic fold of the elements of a 'Producer'
foldM :: (Monad m) => FoldM m a b -> Producer a m () -> m b
foldM (L.FoldM step begin done) = P.foldM step begin done
{-# INLINABLE foldM #-}

-- | Fold all input values
foldAll :: (Monad m) => Fold a b -> Parser a m b
foldAll (L.Fold step begin done) = PP.foldAll step begin done
{-# INLINABLE foldAll #-}

-- | Fold all input values monadically
foldAllM :: (Monad m) => FoldM m a b -> Parser a m b
foldAllM (L.FoldM step begin done) = PP.foldAllM step begin done
{-# INLINABLE foldAllM #-}

-- | Fold each 'Producer' of a 'FreeT'
folds :: (Monad m) => Fold a b -> FreeT (Producer a m) m r -> Producer b m r
folds (L.Fold step begin done) = PP.folds step begin done
{-# INLINABLE folds #-}

-- | Fold each 'Producer' of a 'FreeT', monadically
foldsM :: (Monad m) => FoldM m a b -> FreeT (Producer a m) m r -> Producer b m r
foldsM (L.FoldM step begin done) = PP.foldsM step begin done
{-# INLINABLE foldsM #-}

-- | Reduce the stream of bytes using a strict left fold
foldBytes :: (Monad m) => Fold Word8 b -> Producer ByteString m () -> m b
foldBytes (L.Fold step begin done) = PB.foldBytes step begin done
{-# INLINABLE foldBytes #-}

{- $reexports
    @Control.Foldl@ re-exports the 'Fold' and 'FoldM' types

    @Control.Monad.Trans.Free@ re-exports the 'FreeT' type

    @Pipes@ re-exports the 'Producer' type

    @Pipes.ByteString@ re-exports the 'ByteString' and 'Word8' types

    @Pipes.Parse@ re-exports the 'Parser' type
-}
