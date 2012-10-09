import System.Posix.Eventfd
import System.Environment
import System.Exit
import Control.Concurrent
import Control.Monad

main = do
  xs <- getArgs

  when (length xs < 1) $ do
    putStrLn "Usage, <num>..."
    exitFailure

  efd <- eventfd 0 0  -- TODO: use safe flags
  forkIO $ do
    forM_ xs $ \x -> do
      putStrLn $ "Child writing " ++ x ++ " to efd"
      eventfdWrite efd (fromIntegral $ read  x)

  threadDelay 2000000
  s <- eventfdRead efd
  putStrLn $ "parent read " ++ (show s) ++ " from efd"

      
