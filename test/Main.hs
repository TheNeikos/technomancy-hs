module Main where

import Control.Lens
import Control.Monad.State (execState, replicateM)
import Data (Card (Card, _cardId, _cardProps), CardId (CardId), GameUpdate (AddCardToPlayerZone, AddGlobalZone, AddPlayer, AddZoneToPlayer, MoveCardToZone, fromZone, moveCard, toZone), PlayerId (PlayerId), Properties (Properties), Zone (Zone), ZoneId (ZoneId), ZoneIdentifier (GlobalZone, PlayerZone), defaultGameState, gameZones, playerZones, players, updateGameState, zoneCards)
import Data.HashMap.Lazy (empty, size)
import Data.Maybe
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
    it "adds players and their zones and then cards" $ do
      let newState =
            execState
              ( mapM
                  updateGameState
                  [ AddPlayer (PlayerId 2),
                    AddZoneToPlayer (PlayerId 2) (ZoneId 0),
                    AddCardToPlayerZone (PlayerId 2) (ZoneId 0) (Card {_cardProps = Properties empty, _cardId = CardId 0})
                  ]
              )
              defaultGameState
      (newState ^. players & size) `shouldBe` 1
      (newState ^. players . ix (PlayerId 2) . playerZones & size) `shouldBe` 1
      (newState ^. players . ix (PlayerId 2) . playerZones . ix (ZoneId 0) . zoneCards & length) `shouldBe` 1
    it "moves cards to other zones" $ do
      let newState =
            execState
              ( mapM
                  updateGameState
                  [ AddPlayer (PlayerId 2),
                    AddZoneToPlayer (PlayerId 2) (ZoneId 0),
                    AddCardToPlayerZone
                      (PlayerId 2)
                      (ZoneId 0)
                      ( Card
                          { _cardProps = Properties empty,
                            _cardId = CardId 0
                          }
                      ),
                    AddGlobalZone (ZoneId 23)
                  ]
              )
              defaultGameState

      (newState ^. gameZones . ix (ZoneId 23) . zoneCards & length) `shouldBe` 0
      (newState ^. players . ix (PlayerId 2) . playerZones . ix (ZoneId 0) . zoneCards & length) `shouldBe` 1

      let newnewState =
            execState
              ( updateGameState
                  MoveCardToZone
                    { fromZone = PlayerZone (PlayerId 2) (ZoneId 0),
                      toZone = GlobalZone (ZoneId 23),
                      moveCard = CardId 0
                    }
              )
              newState
      (newnewState ^. gameZones . ix (ZoneId 23) . zoneCards & length) `shouldBe` 1
      (newnewState ^. players . ix (PlayerId 2) . playerZones . ix (ZoneId 0) . zoneCards & length) `shouldBe` 0
