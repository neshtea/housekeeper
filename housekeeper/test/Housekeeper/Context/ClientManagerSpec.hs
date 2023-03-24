{-# LANGUAGE OverloadedStrings #-}

module Housekeeper.Context.ClientManagerSpec where

import Control.Monad.State qualified as ST
import Data.Maybe qualified as Maybe
import Data.Time qualified as Time
import Data.Time.Clock qualified as Clock
import Data.UUID qualified as UUID
import Housekeeper.Capability.EventStore qualified as ES
import Housekeeper.Capability.Time (MonadTime (..))
import Housekeeper.Context.ClientManager qualified as CM
import Test.Hspec as Hspec

newtype MockTime = MockTime {unMockTime :: Clock.UTCTime}

zeroTime :: Clock.UTCTime
zeroTime = Clock.UTCTime (Time.fromGregorian 2023 03 24) (Time.secondsToDiffTime 0)

instance MonadTime (ST.State MockTime) where
  currentTimestamp = do
    (MockTime timestamp) <- ST.get
    -- So we're still getting monotously 'newer' timestamps with each call.
    let nextTimestamp = Clock.addUTCTime (Clock.secondsToNominalDiffTime 1) timestamp
    ST.put (MockTime nextTimestamp)
    pure timestamp

testId :: UUID.UUID
testId = Maybe.fromJust $ UUID.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

testClient :: CM.Client
testClient = CM.Client testId "test" False

runMockTime :: ST.State MockTime a -> a
runMockTime m = ST.evalState m (MockTime zeroTime)

execSpec :: Spec
execSpec = do
  describe "when the client is Nothing" $ do
    describe "and the command is CreateClient" $ do
      it "returns a singleton list with a ClientCreatedEvent" $ do
        runMockTime (CM.exec Nothing (CM.CreateClient testId "test"))
          `shouldBe` Right [ES.EventEnvelope testId zeroTime (CM.ClientCreated "test")]
    describe "and the command is anything but CreateClient" $ do
      it "returns the correct domain error" $ do
        runMockTime (CM.exec Nothing (CM.UpdateClientName testId "new name"))
          `shouldBe` Left (CM.ClientDoesNotExist testId)
  describe "when the client is a Just" $ do
    describe "and the command is CreateClient" $ do
      it "returns a ClientAlreadyCreated domain error" $ do
        runMockTime (CM.exec (Just testClient) (CM.CreateClient testId "new name"))
          `shouldBe` Left (CM.ClientAlreadyCreated testId)
    describe "and the command is valid" $ do
      it "returns the correct sequence of events" $ do
        runMockTime (CM.exec (Just testClient) (CM.UpdateClientName testId "new name"))
          `shouldBe` Right [ES.EventEnvelope testId zeroTime (CM.ClientNameUpdated "new name")]

spec :: Spec
spec = do
  describe "exec" $ do
    execSpec
