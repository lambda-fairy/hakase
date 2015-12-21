module Lemon.Server where


import Control.Concurrent
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word
import Network.Simple.TCP (HostPreference, ServiceName, serve)
import System.IO.Error
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Attoparsec as S
import System.Random.TF.Gen
import System.Random.TF.Init
import System.Random.TF.Instances

import Control.Concurrent.ThreadSet
import Lemon.Protocol
import Lemon.Server.Common
import Lemon.Server.Game


runServer :: HostPreference -> ServiceName -> IO ()
runServer host port =
    withMatchmaker $ \registerPlayer ->
    serve host port $ \(sock, _addr) -> do
        (is, os) <- S.socketToStreams sock
        is' <- S.parserToInputStream messageParser is
        os' <- S.contramap renderMessage os
        nameRef <- newIORef $ error "player name not set"
        closeFlag <- newEmptyMVar
        let playerState = PlayerState {
            playerName = nameRef,
            playerReceive = do
                r <- try (S.read is')
                case r of
                    Left (S.ParseException _) ->
                        runSession (kick InvalidCommand) playerState
                    Right Nothing -> throwM eofError
                    Right (Just m) -> return m,
            playerSend = \m -> S.write (Just m) os',
            playerClose = void $ tryPutMVar closeFlag ()
            }
        runSession (login registerPlayer) playerState
        takeMVar closeFlag
  where
    eofError = mkIOError eofErrorType "end of stream" Nothing Nothing


withMatchmaker :: ((PlayerState -> IO ()) -> IO a) -> IO a
withMatchmaker k =
    bracket forkMatchmaker
            (\(_, tid) -> killThread tid)
            (\(register, _) -> k register)


forkMatchmaker :: IO (PlayerState -> IO (), ThreadId)
forkMatchmaker = do
    chan <- newEmptyMVar
    rng <- newIORef =<< newTFGen
    tid <- forkIO $
        withLobby $ \lobby ->
            withThreadSet $ \ts ->
                loop chan rng lobby ts
    return (putMVar chan, tid)
  where
    loop chan rng lobby ts = do
        seed <- readIORef rng
        players <- readIORef lobby
        -- TODO: Find a better matchmaking algorithm
        if Seq.length players >= 2
            then do
                let (seed', (white, players')) = choose players seed
                    (seed'', (black, players'')) = choose players' seed'
                writeIORef rng seed''
                writeIORef lobby players''
                _ <- fork ts $ do
                    -- TODO: Do something with the results
                    moves <- play numberOfMoves white black `finally`
                        (playerClose white >> playerClose black)
                    print moves
                    return ()
                return ()
            else do
                newPlayer <- takeMVar chan
                modifyIORef' lobby (newPlayer Seq.<|)
        loop chan rng lobby ts

    withLobby :: (IORef (Seq PlayerState) -> IO a) -> IO a
    withLobby = bracket
        (newIORef Seq.empty)
        (\lobby -> readIORef lobby >>= mapM_ playerClose)


login :: (PlayerState -> IO ()) -> Session ()
login registerPlayer = do
    -- TODO: check secret
    (name, _) <- receive >>= \m -> case m of
        Ayy version name secret
            | version == protocolVersion -> return (name, secret)
            | otherwise -> kick MismatchedVersion
        _ -> kick InvalidCommand
    setName name
    send $ Lmao protocolVersion serverVersion
    lift . registerPlayer =<< ask


choose :: RandomGen g => Seq a -> g -> (g, (a, Seq a))
choose xs g =
    let (i, g') = randomR (0, Seq.length xs - 1) g
        (l, r) = Seq.splitAt i xs
        x Seq.:< r' = Seq.viewl r
    in (g', (x, l Seq.>< r'))


-- TODO: make this configurable
numberOfMoves :: Word32
numberOfMoves = 100
