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

  efd <- eventfdSem 0 [] -- TODO: use safe flags
  forM_ xs $ \x -> forkIO $ do
      threadDelay 1000000
      putStrLn $ "Child writing " ++ x ++ " to efd"
      eventfdUpMany efd (fromIntegral $ read  x)

  threadDelay 2000000
  eventfdDown efd
  putStrLn $ "parent read"

      
