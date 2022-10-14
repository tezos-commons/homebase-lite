-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.Metadata.Views
  ( test_currentConfig
  , test_proposalInfo
  ) where

import Test.Tasty (TestTree)

import Morley.Util.Named
import Test.Cleveland
import Test.Morley.Metadata

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types
import Test.Indigo.Contracts.HomebaseLite.AsRPC
import Test.Indigo.Contracts.HomebaseLite.Utils

test_currentConfig :: TestTree
test_currentConfig =
  testScenarioOnEmulator "currentConfig view works" $ scenarioEmulated do
    (admin, contract) <- deployContract
    Showing <$> callOffChainView @Configuration contract "currentConfig" NoParam
      @@== Showing (toRPC $ sConfiguration (defaultStorage (toL1Address admin) Nothing))

test_proposalInfo :: TestTree
test_proposalInfo =
  testScenarioOnEmulator "proposalInfo view works" $ scenarioEmulated do
    (holder, _, contract) <- deployWithFA2
    let uri = #proposal_uri :! URI "ipfs://proposal"
        choices = ["Zero", "One", "Two", "Three"]
    withSender holder do
      transfer contract $ calling (ep @"Propose") (uri, #choices :! choices)
    storage <- getStorage contract
    let proposals = sProposalsRPC storage
    proposal <- getBigMapValue proposals uri
    Showing <$> callOffChainView @ProposalInfo contract "proposalInfo" (ViewParam uri)
      @@== Showing (toRPCProposalInfo proposal)
    expectCustomErrorNoArg #noSuchProposal $
      callOffChainView @ProposalInfo
        contract "proposalInfo" (ViewParam $ #proposal_uri :! URI "ipfs://nonexistent")
