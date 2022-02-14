-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE OverloadedLists #-}

module Test.Indigo.Contracts.HomebaseLite.PropertyTests
  ( hprop_main
  ) where

import Lorentz (Address, toAddress)

import Data.Default
import qualified Data.Map as Map
import Hedgehog (Property, forAll, property, withTests)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Time.Units (sec)

import Hedgehog.Gen.Michelson
import Hedgehog.Gen.Tezos.Core
import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Michelson.Typed.Haskell.Value (bmMap)
import Morley.Tezos.Core
import Morley.Util.Named
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types
import Test.Indigo.Contracts.HomebaseLite.Utils

genericScenario
  :: Natural
  -> (forall caps m. MonadEmulated caps m
      => ContractHandle Parameter Storage () -> m () -> m ())
  -> (forall caps m. MonadEmulated caps m
      => Address
      -> Address
      -> ContractHandle Parameter Storage ()
      -> m ())
  -> Scenario PureM
genericScenario tokenCount expectation f = scenarioEmulated do
  holder <- newAddress "holder"
  let stor = FA2.mkStorage meta [((holder, FA2.TokenId 0), tokenCount)] []
      meta = FA2.mkTokenMetadata "g" "governance" "0"
  fa2 <- originateSimple "FA2" stor (FA2.fa2Contract def)
  (admin, contract) <- deployContractWithConf FA2Config
    { fa2Addr = toAddress fa2
    , fa2TokenId = TokenId 0
    } id
  expectation contract $ f holder admin contract

-- | This checks some invariants on the primary workflow:
--
-- * Admin adds a maintainer
-- * Maintainer changes configuration
-- * Governance token holder posts a proposal
-- * After some delay some votes are cast
--
-- The configuration, the proposal, the delay and votes are randomized, so
-- we can check for a few possible errors here.
hprop_main :: Property
hprop_main = withTests 500 $ property $ do
  let maxDelay = 86400 -- 1 day
      maxMinimumBalance = 1000
  conf@Configuration{..} <- forAll do
    voteDelay <- Gen.integral (Range.linear 0 maxDelay)
    let cVoteDelay = Seconds voteDelay
    cExpireTime <- Seconds <$> Gen.integral (Range.linear voteDelay maxDelay)
    cQuorumThreshold <- Gen.integral $ Range.linear 0 1000
    cMinimumBalance <- Gen.integral $ Range.linear 0 maxMinimumBalance
    pure Configuration{..}
  let tokenCount = maxMinimumBalance `div` 2
  choices <- forAll $ Gen.list (Range.linear 0 100) genMText
  uri <- forAll $ (#proposal_uri :!) . URI <$> genMText
  votes <- forAll $ Gen.list (Range.linear 0 100)
    $ Gen.integral (Range.constant 0 100)
  let maxChoice = fromIntegralOverflowing $ length choices - 1
      validVotes = filter (<= maxChoice) votes
  curTime <- forAll genTimestamp
  delay <- forAll $ Gen.integral (Range.linear 0 maxDelay)
  let expectation :: MonadEmulated caps m
                  => ContractHandle Parameter Storage () -> m () -> m ()
      expectation contract
        | null choices = expectCustomErrorNoArg #emptyChoices
        | tokenCount < cMinimumBalance = expectCustomErrorNoArg #notEnoughTokens
        | null validVotes = verifyStorage contract \stor -> sVotes stor @== def
        | delay < unSeconds cVoteDelay = expectCustomErrorNoArg #proposalNotYetActive
        | delay > unSeconds cExpireTime = expectCustomErrorNoArg #proposalExpired
        | otherwise = verifyStorage contract \stor ->
            sort (Map.elems . bmMap . sVotes $ stor) @== (#vote_choice :!) <$> sort validVotes

      verifyStorage
        :: forall caps m'. MonadEmulated caps m'
        => ContractHandle Parameter Storage ()
        -> (forall m. MonadCleveland caps m => Storage -> m ())
        -> (forall m. MonadCleveland caps m => m ())
        -> m' ()
      verifyStorage contract checks action = do
          action
          stor <- getFullStorage contract
          checkProposalInfo stor
          void $ checks stor

      checkProposalInfo :: MonadCleveland caps m => Storage -> m ()
      checkProposalInfo stor = do
        ProposalInfo{..} <-
          maybe (failure "Proposal not found in storage")
            pure $ Map.lookup uri (bmMap (sProposals stor))
        piQuorumThreshold @== cQuorumThreshold
        piStartsAt @== timestampPlusSeconds curTime  (fromIntegral $ unSeconds cVoteDelay)
        piExpiresAt @== timestampPlusSeconds curTime (fromIntegral $ unSeconds cExpireTime)
        piChoices @== choices
  testScenarioProps $ withInitialNow curTime $
    genericScenario tokenCount expectation \holder admin contract -> do
      maintainer <- newAddress "maintainer"
      withSender admin do
        call contract (Call @"Add_maintainers") [maintainer]
      withSender maintainer do
        call contract (Call @"Configure") conf
      withSender holder do
        call contract (Call @"Propose") (uri, #choices :! choices)
      advanceTime $ sec (fromIntegralOverflowing @Natural delay)
      for_ votes \vote -> do
        voter <- newAddress auto
        withSender voter do
          call contract (Call @"Vote") (uri, #choice_index :! vote)
            & if vote > maxChoice
              then expectCustomErrorNoArg #noSuchChoice
              else id
