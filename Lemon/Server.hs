module Lemon.Server where


import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Data.Word

import Lemon.Protocol


data PlayerState = PlayerState {
    playerName :: Text,
    playerReceive :: IO Message,
    playerSend :: Message -> IO ()
    }


type Game = ReaderT (PlayerState, PlayerState) IO


data GameOver = GameOver
    deriving Show

instance Exception GameOver


getPlayer :: Player -> Game PlayerState
getPlayer White = asks fst
getPlayer Black = asks snd


name :: Player -> Game Text
name = fmap playerName . getPlayer


receive :: Player -> Game Message
receive = lift . playerReceive <=< getPlayer


send :: Player -> Message -> Game ()
send player message = do
    s <- getPlayer player
    lift $ playerSend s message


play :: Word32 -> Game [(Move, Move)]
play totalMoves = bracket_ introduction denouement $ loop totalMoves
  where
    introduction = do
        startMessage <- Start totalMoves <$> name White <*> name Black
        send White startMessage
        send Black startMessage
    denouement = do
        send White Done
        send Black Done


loop :: Word32 -> Game [(Move, Move)]
loop movesLeft = replicateM (fromIntegral movesLeft) $ do
    whiteMove <- receiveMove White
    blackMove <- receiveMove Black
    return (whiteMove, blackMove)
  where
    receiveMove player = receive player >>= \m -> case m of
        Move player' move | player == player' ->
            send (other player) m >> return move
        _ -> kick player InvalidCommand


kick :: Player -> KnownError -> Game a
kick player err = do
    send player $ Error (KnownError err)
    throwM GameOver
