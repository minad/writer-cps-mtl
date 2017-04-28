{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Cont
import Control.Monad.Writer.Class
import Control.Monad.State.Class

import qualified Control.Monad.RWS.Strict as S
import qualified Control.Monad.RWS.CPS    as CPS

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain suite

test_accRollbackContRwst :: TestTree
test_accRollbackContRwst =
  testCase "Accumulator rollback in MonadCont for RWST" $
    assertEqual "" f g
  where
    fm :: (MonadCont m, MonadState Bool m, MonadWriter String m) => m ()
    fm = do
      tell "outside;"
      callCC $ \c -> do
        put False
        tell "inside;"
        c ()

    f, g :: ((), Bool, String)
    f = runCont   (S.runRWST fm () True) id
    g = runCont (CPS.runRWST fm () True) id

suite :: TestTree
suite = testGroup "writer-cps" [ test_accRollbackContRwst ]
