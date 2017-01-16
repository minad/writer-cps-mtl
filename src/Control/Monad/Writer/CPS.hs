{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Writer.CPS
-- Copyright   :  (c) Daniel Mendler 2016,
--                (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Stricter writer monad using continuation-passing-style for the
-- writer output.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/pubs/springschool.html>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.Writer.CPS (
  -- * The Writer monad
  Writer,
  runWriter,
  execWriter,
  mapWriter,
  -- * The WriterT monad transformer
  WriterT(..),
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

import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Writer.CPS
import qualified Control.Monad.Trans.Writer.CPS as CPS

-- Orphan instances

instance (Monad m, Monoid w) => MonadWriter w (WriterT w m) where
  writer = CPS.writer
  tell = CPS.tell
  listen = CPS.listen
  pass = CPS.pass

instance MonadError e m => MonadError e (WriterT w m) where
  throwError = lift . throwError
  catchError = CPS.liftCatch catchError

instance MonadCont m => MonadCont (WriterT w m) where
  callCC = CPS.liftCallCC callCC

instance (MonadReader r m, Monoid w) => MonadReader r (WriterT w m) where
  ask    = lift ask
  local  = CPS.mapWriterT . local
  reader = lift . reader

instance MonadState s m => MonadState s (WriterT w m) where
  get = lift get
  put = lift . put
  state = lift . state
