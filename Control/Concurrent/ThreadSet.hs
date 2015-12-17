module Control.Concurrent.ThreadSet (
    withThreadSet,
    forkIOWithThreadSet,
    fork,
    newThreadSet,
    killThreadSet,
    ) where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IORef


newtype ThreadSet = TS (IORef (Set ThreadId))


withThreadSet :: (ThreadSet -> IO a) -> IO a
withThreadSet = bracket newThreadSet killThreadSet


forkIOWithThreadSet :: (ThreadSet -> IO ()) -> IO ThreadId
forkIOWithThreadSet k = do
    ts <- newThreadSet
    forkFinally (k ts) (\_ -> killThreadSet ts)


fork :: ThreadSet -> IO () -> IO ThreadId
fork (TS ref) action =
    mask $ \restore ->
        forkIO $ do
            tid <- myThreadId
            atomicModifyIORef' ref $ \ts -> (Set.insert tid ts, ())
            restore action `finally`
                atomicModifyIORef' ref (\ts -> (Set.delete tid ts, ()))


newThreadSet :: IO ThreadSet
newThreadSet = TS <$> newIORef Set.empty


killThreadSet :: ThreadSet -> IO ()
killThreadSet = mapM_ killThread <=< clearTS
  where
    clearTS (TS ref) =
        atomicModifyIORef ref $ \ts -> (Set.empty, Set.toList ts)
