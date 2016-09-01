{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Writer.CPS (
  -- * The Writer monad
  Writer,
  runWriter,
  execWriter,
  mapWriter,
  -- * The WriterT monad transformer
  WriterT,
  runWriterT,
  execWriterT,
  mapWriterT,
  module X
) where

import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.Trans as X
import Control.Monad.Writer.Class as X
import Data.Monoid as X

import Control.Monad.Trans.Writer.CPS hiding (writer, listen, pass)
import qualified Control.Monad.Trans.Writer.CPS as CPS

-- Orphan instance
instance (Monoid w, Monad m) => MonadWriter w (CPS.WriterT w m) where
  writer = CPS.writer
  listen = CPS.listen
  pass = CPS.pass
