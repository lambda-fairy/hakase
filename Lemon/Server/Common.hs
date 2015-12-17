module Lemon.Server.Common where


import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Text (Text)

import Lemon.Protocol


type Session = ReaderT PlayerState IO


data PlayerState = PlayerState {
    playerName :: IORef Text,
    playerReceive :: IO Message,
    playerSend :: Message -> IO ()
    }


runSession :: Session a -> PlayerState -> IO a
runSession = runReaderT


getName :: Session Text
getName = lift . readIORef =<< asks playerName


setName :: Text -> Session ()
setName name = do
    ref <- asks playerName
    lift $ atomicWriteIORef ref name


receive :: Session Message
receive = lift =<< asks playerReceive


send :: Message -> Session ()
send message = do
    k <- asks playerSend
    lift $ k message


kick :: KnownError -> Session a
kick err = do
    send $ Error (KnownError err)
    throwM GameOver


data GameOver = GameOver
    deriving Show

instance Exception GameOver
