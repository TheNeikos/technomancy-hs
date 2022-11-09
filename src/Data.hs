{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data where

import Control.Lens
import Control.Monad.State
import Data.HashMap.Lazy (HashMap, empty)
import Data.Hashable
import GHC.Generics (Generic)

data PropertyValue = DataInt Int | DataString String deriving (Show)

newtype Properties = Properties (HashMap String PropertyValue) deriving (Show)

noProperties :: Properties
noProperties = Properties empty

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

data Zone = Zone
  { _zoneCards :: [Card]
  }
  deriving (Show)

makeLenses ''Zone

data Player = Player
  { _playerZones :: HashMap ZoneId Zone,
    _playerProperties :: Properties,
    _playerId :: PlayerId
  }
  deriving (Show)

makeLenses ''Player

defaultPlayer :: PlayerId -> Player
defaultPlayer id = Player {_playerZones = empty, _playerProperties = noProperties, _playerId = id}

data GameState = GameState
  { _gameZones :: HashMap ZoneId Zone,
    _players :: HashMap PlayerId Player
  }

defaultGameState :: GameState
defaultGameState = GameState {_gameZones = empty, _players = empty}

makeLenses ''GameState

data ZoneIdentifier
  = PlayerZone PlayerId ZoneId
  | GlobalZone ZoneId

data GameUpdate
  = AddGlobalZone ZoneId
  | AddPlayer PlayerId
  | AddZoneToPlayer PlayerId ZoneId
  | AddCardToPlayerZone PlayerId ZoneId Card
  | MoveCardToZone {fromZone :: ZoneIdentifier, toZone :: ZoneIdentifier, moveCard :: CardId}

findElement :: (Card -> Bool) -> [Card] -> Maybe Card
findElement check = find' id
  where
    find' rest [] = Nothing
    find' prefix (x : xs)
      | check x = Just x
      | otherwise = find' (prefix . (x :)) xs

removeElement :: (a -> Bool) -> [a] -> [a]
removeElement check = find' id
  where
    find' prefix [] = prefix []
    find' prefix (x : xs)
      | check x = prefix xs
      | otherwise = find' (prefix . (x :)) xs

updateGameState :: GameUpdate -> State GameState ()
updateGameState (AddGlobalZone zoneid) = gameZones . at zoneid ?= Zone []
updateGameState (AddPlayer playerid) = players . at playerid ?= defaultPlayer playerid
updateGameState (AddZoneToPlayer playerid zoneid) = players . at playerid . _Just . playerZones . at zoneid ?= Zone []
updateGameState (AddCardToPlayerZone playerid zoneid card) = modify $ players . ix playerid . playerZones . ix zoneid . zoneCards <>~ [card]
updateGameState (MoveCardToZone {fromZone = _fromZone, toZone = _toZone, moveCard = _moveCard}) = do
  cards <- get
  let card = findElem $ cards ^. getZone _fromZone . zoneCards
  getZone _fromZone . zoneCards %= removeElement ((_moveCard ==) . _cardId)
  case card of
    Just card -> getZone _toZone . zoneCards <>= [card]
    Nothing -> error "Could not find card"
  where
    findElem :: [Card] -> Maybe Card
    findElem = findElement ((_moveCard ==) . _cardId)
    getZone :: ZoneIdentifier -> Simple Traversal GameState Zone
    getZone (PlayerZone playerid zoneid) = players . at playerid . _Just . playerZones . ix zoneid
    getZone (GlobalZone zoneid) = gameZones . ix zoneid
