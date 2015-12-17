module Lemon.Server.Game (play) where


import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Reader
import Data.Word

import Lemon.Protocol
import Lemon.Server.Common


play :: Word32 -> PlayerState -> PlayerState -> IO [(Move, Move)]
play totalMoves white black = runReaderT (playGame totalMoves) (white, black)


type Game = ReaderT (PlayerState, PlayerState) IO


(.:) :: Player -> Session a -> Game a
(.:) White = withReaderT fst
(.:) Black = withReaderT snd
infix 5 .:


playGame :: Word32 -> Game [(Move, Move)]
playGame totalMoves = bracket_ exposition denouement $ loop totalMoves
  where
    exposition = do
        startMessage <- Start totalMoves <$> White .: getName <*> Black .: getName
        White .: send startMessage
        Black .: send startMessage
    denouement = do
        White .: send Done
        Black .: send Done


loop :: Word32 -> Game [(Move, Move)]
loop totalMoves = replicateM (fromIntegral totalMoves) $ do
    whiteMove <- receiveMove White
    blackMove <- receiveMove Black
    return (whiteMove, blackMove)
  where
    receiveMove player = player .: receive >>= \m -> case m of
        Move player' move | player == player' -> do
            other player .: send m
            return move
        _ -> player .: kick InvalidCommand
