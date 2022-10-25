-- |

module HelloMaybeT where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

foo :: IO ()
foo = do
  mevent <- do
    mresult <- getResult
    case mresult of
      Nothing -> pure Nothing
      Just re -> do
        stage <- getStage re
        if stage == 'o'
          then createEvent
          else pure Nothing
  mapM_ putStrLn mevent

bar :: IO ()
bar = do
  mevent <- runMaybeT $ do
    re <- MaybeT getResult
    stage <- lift (getStage re)
    guard $ stage == 'o'
    MaybeT createEvent
  mapM_ putStrLn mevent

getResult :: IO (Maybe Int)
getResult = undefined

getStage :: Int -> IO Char
getStage = undefined

createEvent :: IO (Maybe String)
createEvent = undefined
