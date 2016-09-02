{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.RWS.CPS
-- Copyright   :  (c) Daniel Mendler 2016,
--                (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  mail@daniel-mendler.de
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- Stricter RWS monad using continuation-passing-style for the
-- writer output.
--
--      Inspired by the paper
--      /Functional Programming with Overloading and Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Control.Monad.RWS.CPS (
  -- * The RWS monad
  RWS,
  rws,
  runRWS,
  evalRWS,
  execRWS,
  mapRWS,
  withRWS,
  -- * The RWST monad transformer
  RWST,
  runRWST,
  evalRWST,
  execRWST,
  mapRWST,
  withRWST,
  -- * Strict Reader-writer-state monads
  module X
) where

import Control.Monad as X
import Control.Monad.Fix as X
import Control.Monad.RWS.Class as X
import Control.Monad.Trans as X
import Data.Monoid as X

import Control.Monad.Trans.RWS.CPS hiding (censor, tell, writer, listen, pass, ask,
                                           local, reader, get, put, state, listens,
                                           asks, modify, gets)
import qualified Control.Monad.Trans.RWS.CPS as CPS

-- Orphan instance
instance (Monoid w, Monad m) => MonadWriter w (RWST r w s m) where
  tell = CPS.tell
  writer = CPS.writer
  listen = CPS.listen
  pass = CPS.pass

-- Orphan instance
instance Monad m => MonadReader r (RWST r w s m) where
  ask = CPS.ask
  local = CPS.local
  reader = CPS.reader

-- Orphan instance
instance Monad m => MonadState s (RWST r w s m) where
  get = CPS.get
  put = CPS.put
  state = CPS.state
