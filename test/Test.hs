{-# LANGUAGE FlexibleInstances #-}
import Test.HUnit hiding (Test)
import Test.Hspec
import Test.Hspec.HUnit ()
import Test.Hspec.Core (Result(..), Example(..))

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

import System.Posix.Eventfd

main :: IO ()
main = hspec specs

specs :: Spec
specs = describe "eventfd-read" $ do
  it "create-close" $ do
    fd <- eventfd 0 []
    eventfdClose fd

  it "create-num-close" $ do
    fd <- eventfd (fromIntegral num) []
    num' <- eventfdRead fd
    num @=? num'
    eventfdClose fd

  it "create-write-read-close" $ do
    fd <- eventfd 0 []
    eventfdWrite fd num
    num' <- eventfdRead fd
    num @=? num'
    eventfdClose fd

  it "locks on empty read" $ withEventfd 0 [] $ \fd -> do
    r    <- race (eventfdRead fd) (threadDelay 100)
    return $ either (const $ Fail "read was not blocked") (const $ Success)  r
  it "non sem read set value to zero" $ withEventfd 100 [] $ \fd -> do
    _ <- eventfdRead fd
    r    <- race (eventfdRead fd) (threadDelay 100)
    return $ either (const $ Fail "read was not blocked") (const $ Success) r
  it "sem read descrement value by one" $ withEventfd 70 [efdSemaphore] $ \fd -> do
    x <- eventfdRead fd
    x @=? 1
    mapM (\_ -> eventfdRead fd >>= (@=? 1)) [1..69]
    r    <- race (eventfdRead fd) (threadDelay 100)
    return $ either (const $ Fail "read was not blocked") (const $ Success) r
  it "write that exceed maximum will block" $ withEventfd 0 [] $ \fd -> do
    eventfdWrite fd 0xfffffffffffffffe
    r <- race (eventfdWrite fd 2) (threadDelay 100)
    r @=? Right ()
    r1 <- race (eventfdWrite fd 2) (eventfdRead fd >> threadDelay 100)
    r1 @=? Left 8 -- 0xfffffffffffffffe
    r2 <- eventfdRead fd
    r2 @=? 2

  where 
    num = 42
    withEventfd i f= bracket (eventfd i f) (eventfdClose)





instance Example (IO Result) where
  evaluateExample _ r = r
