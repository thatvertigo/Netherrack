module Lib
    ( main
    ) where

import Rcon
import Env
import Watch
import Control.Monad (join, forever)
import Control.Monad.Trans.Class
import UnliftIO.Concurrent (forkIO)
import UnliftIO
import Control.Monad.Trans.Reader (asks)
import Discord.Bot (launchBot)

main :: IO ()
main = either (print . ("Error: " <>)) pure =<< join (runWithEnv $ do
    chan <- asks logChan 
    _ <- forkIO watchLogs
    launchBot)
