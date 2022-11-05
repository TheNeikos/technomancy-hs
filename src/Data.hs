{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data where

import Control.Lens
import Control.Monad.State
import Data.HashMap.Lazy (HashMap, empty)
import Data.Hashable
import GHC.Generics (Generic)

data PropertyValue = DataInt Int | DataString String deriving (Show)

newtype Properties = Properties (HashMap String PropertyValue) deriving (Show)

newtype CardId = CardId Int deriving (Show, Eq, Generic)

instance Hashable CardId

newtype ZoneId = ZoneId Int deriving (Show, Eq, Generic)

instance Hashable ZoneId

newtype PlayerId = PlayerId Int deriving (Show, Eq, Generic)

instance Hashable PlayerId

data Card = Card
  { _cardProps :: Properties,
    _cardId :: CardId
  }
  deriving (Show)

makeLenses ''Card

newtype Zone = Zone [Card] deriving (Show)

data Player = Player
  { _playerZones :: HashMap ZoneId Zone,
    _playerId :: PlayerId
  }
  deriving (Show)

makeLenses ''Player

defaultPlayer :: PlayerId -> Player
defaultPlayer id = Player {_playerZones = empty, _playerId = id}

data GameState = GameState
  { _gameZones :: HashMap ZoneId Zone,
    _players :: HashMap PlayerId Player
  }

defaultGameState :: GameState
defaultGameState = GameState {_gameZones = empty, _players = empty}

makeLenses ''GameState

data GameUpdate
  = AddGlobalZone ZoneId
  | AddPlayer PlayerId
  | AddZoneToPlayer PlayerId ZoneId

updateGameState :: GameUpdate -> State GameState ()
updateGameState (AddGlobalZone zoneid) = gameZones . at zoneid ?= Zone []
updateGameState (AddPlayer playerid) = players . at playerid ?= defaultPlayer playerid
updateGameState (AddZoneToPlayer playerid zoneid) = players . at playerid . _Just . playerZones . at zoneid ?= Zone []
