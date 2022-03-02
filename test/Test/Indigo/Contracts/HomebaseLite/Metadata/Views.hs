-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.Metadata.Views
  ( test_currentConfig
  , test_proposalInfo
  ) where

import Data.Text (isInfixOf)
import Test.Tasty (TestTree)

import Morley.Metadata
import Morley.Util.Named
import Test.Cleveland
import Test.Cleveland.Internal.Exceptions (WithCallStack(..))
import Test.Cleveland.Internal.Pure (TestError(CustomTestError))
import Test.Morley.Metadata

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types
import Test.Indigo.Contracts.HomebaseLite.AsRPC
import Test.Indigo.Contracts.HomebaseLite.Utils

test_currentConfig :: TestTree
test_currentConfig =
  testScenarioOnEmulator "currentConfig view works" $ scenarioEmulated do
    (admin, contract) <- deployContract
    callOffChainView contract "currentConfig" NoParam
      @@== sConfiguration (defaultStorage admin Nothing)

test_proposalInfo :: TestTree
test_proposalInfo =
  testScenarioOnEmulator "proposalInfo view works" $ scenarioEmulated do
    (holder, _, contract) <- deployWithFA2
    let uri = #proposal_uri :! URI "ipfs://proposal"
        choices = ["Zero", "One", "Two", "Three"]
    withSender holder do
      call contract (Call @"Propose") (uri, #choices :! choices)
    storage <- getStorage contract
    let proposals = sProposalsRPC storage
    proposal <- getBigMapValue proposals uri
    callOffChainView contract "proposalInfo" (ViewParam uri)
      @@== proposal
    res <- attempt $
      callOffChainView @ProposalInfo
        contract "proposalInfo" (ViewParam $ #proposal_uri :! URI "ipfs://nonexistent")
    case res of
      Left (WithCallStack _ err) -> case fromException err of
        Just (CustomTestError txt)
          | "Reached FAILWITH instruction with \"NoSuchProposal\"" `isInfixOf` txt -> pass
        _ -> failure $
          "Expected a noSuchProposal error, but got " <> fromString (displayException err)
      Right _ -> failure "Expected a noSuchProposal error, but the call succeeded"
