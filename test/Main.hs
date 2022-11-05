module Main where

import Control.Lens
import Control.Monad.State (execState, replicateM)
import Data (GameUpdate (AddGlobalZone, AddPlayer, AddZoneToPlayer), PlayerId (PlayerId), Zone (Zone), ZoneId (ZoneId), defaultGameState, gameZones, playerZones, players, updateGameState)
import Data.HashMap.Lazy (empty, size)
import Test.Hspec (describe, hspec, it, shouldBe)

main = hspec $ do
  describe "Simple Game Update" $ do
    it "creates an empty game" $ do
      (defaultGameState ^. players & size) `shouldBe` 0
      (defaultGameState ^. gameZones & size) `shouldBe` 0
    it "adds zones" $ do
      let newState = execState (updateGameState (AddGlobalZone $ ZoneId 1)) defaultGameState
      (newState ^. gameZones & size) `shouldBe` 1
    it "adds players and their zones" $ do
      let newState = execState (mapM updateGameState [AddPlayer (PlayerId 2), AddZoneToPlayer (PlayerId 2) (ZoneId 0)]) defaultGameState
      (newState ^. players & size) `shouldBe` 1
      (newState ^. players . at (PlayerId 2) . _Just . playerZones & size) `shouldBe` 1
