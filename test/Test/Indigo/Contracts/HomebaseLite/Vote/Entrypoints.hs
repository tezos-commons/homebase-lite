-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Test.Indigo.Contracts.HomebaseLite.Vote.Entrypoints
  ( test_propose
  , test_vote
  ) where

import Lorentz (MText, mkBigMap)

import Test.Tasty (TestTree, testGroup)

import Morley.Tezos.Core
import Morley.Util.Named
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types
import Test.Indigo.Contracts.HomebaseLite.AsRPC
import Test.Indigo.Contracts.HomebaseLite.Utils

test_propose :: TestTree
test_propose = testGroup "propose entrypoint"
  [ testScenario "works" $ scenario do
      (holder, _, contract) <- deployWithFA2
      lastLevel <- getLevel
      timestamp <- getNow
      withSender holder do
        transfer contract $ calling (ep @"Propose") (uri, #choices :! choices)
      storage <- getStorage contract
      let config = sConfigurationRPC storage
          proposals = sProposalsRPC storage
      proposal <- getBigMapValue proposals uri
      piChoices proposal @== choices
      piQuorumThreshold proposal @== cQuorumThresholdRPC config
      -- this is not a very precise test, but can't really do much better
      -- TODO [morley#687]: these tests can be much more precise on the emulator
      -- but we currently don't have a good way to detect where we're running.
      checkCompares (piLevel proposal) (>=) lastLevel
      checkCompares (piStartsAt proposal) (>=) timestamp
      checkCompares (piExpiresAt proposal) (>=) timestamp
  , testScenario "fails on not enough tokens" $ scenario do
      (_, _, contract) <- deployWithFA2
      (transfer contract $ calling (ep @"Propose") (uri, #choices :! choices))
        & expectCustomErrorNoArg #notEnoughTokens
  , testScenario "fails on empty choice list" $ scenario do
      (holder, _, contract) <- deployWithFA2
      withSender holder do
        (transfer contract $ calling (ep @"Propose") (uri, #choices :! []))
          & expectCustomErrorNoArg #emptyChoices
  , testScenario "fails on duplicate proposal" $ scenario do
      (holder, _, contract) <- deployWithFA2
      withSender holder do
        transfer contract $ calling (ep @"Propose") (uri, #choices :! choices)
        (transfer contract $ calling (ep @"Propose") (uri, #choices :! ["Other", "Choices"]))
          & expectCustomErrorNoArg #duplicateProposal
  , testScenario "fails on invalid configuration" $ scenario do
      (_, contract) <- deployContract
      (transfer contract $ calling (ep @"Propose") (uri, #choices :! choices))
        & expectCustomErrorNoArg #noFA2Contract
  ]
  where
    uri = #proposal_uri :! URI "ipfs://proposal"
    choices = ["Zero", "One", "Two", "Three"]

test_vote :: IO TestTree
test_vote = do
  time <- getCurrentTime
  let myScenario :: Natural -> MText -> Int -> Integer -> Maybe LabelEx -> Scenario m
      myScenario idx uri nvotes delta expectation
        = scenario $ maybe id (\(LabelEx label) -> expectCustomErrorNoArg label) expectation do
          ts <- getNow
          (_, contract) <- deployContract' $ addProposal ts
          participant <- newAddress "participant"
          withSender participant $ replicateM_ nvotes do
            transfer contract $ calling (ep @"Vote") (#proposal_uri :! URI uri, #choice_index :! idx)
          votes <- sVotesRPC <$> getStorage contract
          getBigMapValue votes (#proposal_uri :! URI uri, #voter_address :! (toAddress participant))
            @@== #vote_choice :! idx
        where
          addProposal ts s@Storage{..} = s{
              sProposals = mkBigMap [
                (#proposal_uri :! URI "ipfs://proposal", ProposalInfo
                  { piLevel = 0
                  , piChoices = ["Zero", "One", "Two", "Three"]
                  , piStartsAt =
                    cratedTime `timestampPlusSeconds`
                      fromIntegral (unSeconds (cVoteDelay sConfiguration))
                  , piExpiresAt =
                    cratedTime `timestampPlusSeconds`
                      fromIntegral (unSeconds (cExpireTime sConfiguration))
                  , piQuorumThreshold = cQuorumThreshold sConfiguration
                  })
                ]
            }
            where cratedTime = ts `timestampPlusSeconds` (-delta)
  pure $ testGroup "vote entrypoint" $ (flip concatMap tests \(name, choice, uri, nvotes, delta, err) ->
    [ testScenarioOnNetwork name $ myScenario choice uri nvotes delta err
    , testScenarioOnEmulator name $ withInitialNow time $ myScenario choice uri nvotes delta err
    ])
  where
    minDelay = 1200 -- 20m
    maxDelay = 4800 -- 1h20m
    tests =
      [ ("works", 3, "ipfs://proposal", 1, minDelay, Nothing)
      , ("works with index 0", 0, "ipfs://proposal", 1, minDelay, Nothing)
      , ("fails on nonexistent proposal", 0, "ipfs://nonexistent", 1, minDelay, Just $ LabelEx #noSuchProposal)
      , ("fails on nonexistent choice", 4, "ipfs://proposal", 1, minDelay, Just $ LabelEx #noSuchChoice)
      , ("fails on duplicate vote", 1, "ipfs://proposal", 2, minDelay, Just $ LabelEx #alreadyVoted)
      , ("fails when proposal is not yet active", 2, "ipfs://proposal", 1, 0, Just $ LabelEx #proposalNotYetActive)
      , ("fails when proposal is expired", 3, "ipfs://proposal", 1, maxDelay, Just $ LabelEx #proposalExpired)
      ]
