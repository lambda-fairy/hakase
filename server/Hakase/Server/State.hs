{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Hakase.Server.State
    ( ServerState()
    , defaultServerState
    , RegisterPlayer(..)
    , CheckPlayer(..)
    , RecordBattle(..)
    , QueryBattle(..)
    , PlayerBattles(..)
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.SafeCopy
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Hakase.Server.Common


-- Orphan instances!! >_<
deriveSafeCopy 0 'base ''Move
deriveSafeCopy 0 'base ''Battle


-- | Represents the persistent state maintained by the server.
--
-- This comprises the set of registered players, as well as a log of all past
-- battles.
data ServerState = ServerState
    { statePlayers :: !(Map Text Text)
        -- ^ The set of registered players. Maps each player to their secret.
    , stateBattles :: !(IntMap Battle)
        -- ^ Maps every battle ID to a corresponding battle.
    } deriving Show

deriveSafeCopy 0 'base ''ServerState

-- | An empty server state.
defaultServerState :: ServerState
defaultServerState = ServerState
    { statePlayers = Map.empty
    , stateBattles = IntMap.empty
    }


-- | Try to register a player with the given name and secret.
--
-- Returns True on success, or False if the name is already taken.
registerPlayer :: Text -> Text -> Update ServerState Bool
registerPlayer name secret = state $ \s ->
    if Map.member name (statePlayers s)
        then (False, s)
        else (True, s { statePlayers = Map.insert name secret (statePlayers s) })

-- | Check if a player has been registered, and that their secret matches that
-- in the database.
checkPlayer :: Text -> Text -> Query ServerState Bool
checkPlayer name secret =
    (== Just secret) . Map.lookup name . statePlayers <$> ask

-- | Record the outcome of an epic battle.
recordBattle :: Battle -> Update ServerState ()
recordBattle battle = modify $ \s ->
    let battleId
            | IntMap.null (stateBattles s) = 0
            | otherwise = 1 + fst (IntMap.findMax (stateBattles s))
    in  s { stateBattles = IntMap.insert battleId battle (stateBattles s) }

-- | Look up information on a single battle.
queryBattle :: Int -> Query ServerState (Maybe Battle)
queryBattle battleId = IntMap.lookup battleId . stateBattles <$> ask

-- | Look up all battles fought by a particular player.
playerBattles :: Text -> Query ServerState [(Int, Battle)]
playerBattles name = IntMap.toList
    . IntMap.filter (\b -> battleWhite b == name || battleBlack b == name)
    . stateBattles <$> ask

makeAcidic ''ServerState
    [ 'registerPlayer
    , 'checkPlayer
    , 'recordBattle
    , 'queryBattle
    , 'playerBattles
    ]
