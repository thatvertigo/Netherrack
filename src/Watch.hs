module Watch where

import System.FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List ( isSuffixOf )
import qualified Log.Event as L
import Data.Functor
import Control.Monad.Trans.Reader (ReaderT, asks)
import Env
import Control.Monad.Trans.Class (MonadTrans(lift))
import UnliftIO (writeTChan, atomically)

watchLogs :: ReaderT Env IO ()
watchLogs = do
    (path, filename) <- (,) <$> asks (logPath . config) <*> asks (logFilename . config)
    chan <- asks logChan 
    lift $ withManager $ 
        \mgr -> do
            _ <- watchDir
                mgr          -- manager
                path          -- directory to watch
                (\case
                    Modified { eventPath = ep, eventTime = _, eventIsDirectory = IsFile } -> ("/" <> filename) `isSuffixOf` ep
                    _ -> False) -- predicate
                (const $ processLine (path <> "/" <> filename) >>= mapM_ (atomically . writeTChan chan))        -- action

            -- sleep forever (until interrupted)
            forever $ threadDelay 1000000

processLine :: String -> IO [L.Event]
processLine fullPath = lines <$> readFile fullPath <&> \case
    []  -> []
    xs -> L.findEvents $ last xs
