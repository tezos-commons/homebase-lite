-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Vote
  ( propose
  , vote
  , verifyMinBalance
  ) where

import Indigo hiding ((*))

import Morley.Util.Interpolate (itu)

import Indigo.Contracts.HomebaseLite.Impl.Utils
import Indigo.Contracts.HomebaseLite.Types

propose :: (IsNotInView, HasSideEffects) => HomebaseLiteEntrypoint ("proposal_uri" :! URI, "choices" :! [MText])
propose par = do
  description [itu|
    Called by a `participant` to submit a new proposal.
    Takes an IPFS URI and a list of strings for the available choices as input.
    A new proposal for the given IPFS URI will be initialized using the current
    **configuration options** (see `configure`).
    |]
  choices <- new$ par #! #choices
  when (size choices == 0 nat) do
    failCustomNoArg @() #emptyChoices
  uri <- new$ fst par
  proposals <- new$ storage #! #sProposals
  when (mem uri proposals) do
    failCustomNoArg @() #duplicateProposal
  selfRef <- selfCalling @Parameter (Call @"Verify_min_balance")
  mContractRef <- contractCalling @FA2Parameter (Call @"Balance_of") $
    storage #! #sFA2Config #! #fa2Addr
  let param = construct (reqs, varExpr selfRef)
      reqs = (construct (sender, storage #! #sFA2Config #! #fa2TokenId)) .: nil
  ifNone mContractRef (failCustomNoArg @() #noFA2Contract) $
    transferTokens param (0 mutez)
  time <- new$ now
  let startTime = time + (toInt . (#! #unSeconds)) (storage #! #sConfiguration #! #cVoteDelay)
      endTime = time + (toInt . (#! #unSeconds)) (storage #! #sConfiguration #! #cExpireTime)
  updateStorageField @Storage #sProposals $
    pure . insert
      ( uri
      , construct
        ( level
        , startTime
        , endTime
        , storage #! #sConfiguration #! #cQuorumThreshold
        , varExpr choices
        )
      )

verifyMinBalance :: HomebaseLiteEntrypoint [BalanceResponseItem]
verifyMinBalance bris = do
  description [itu|
    Callback for the `balance_of` CPS view of FA2,
    used to check the governance token's balance of a `participant` that used the
    `propose` entrypoint.
    |]
  forEach bris \bri -> do
    when (bri #! #briBalance < storage #! #sConfiguration #! #cMinimumBalance) do
      failCustomNoArg @() #notEnoughTokens

[errorDocArg| "notEnoughTokens" exception "Some of the `balance`s in the list are
less than `minimum_balance`"|]
[errorDocArg| "duplicateProposal" exception "A proposal for the same IPFS URI already exists"|]
[errorDocArg| "emptyChoices" bad-argument "The proposal has no choices, i.e. the provided list of
available choices is empty"|]
[errorDocArg| "noFA2Contract" contract-internal "Configured FA2 contract not found"|]

vote :: HomebaseLiteEntrypoint ("proposal_uri" :! URI, "choice_index" :! Natural)
vote par = do
  description [itu|
    Called by a `participant` to cast a vote on a proposal.
    Takes the proposal's IPFS URI and a choice index (zero-based).
    |]
  uri <- new$ fst par
  ifNone ((storage #! #sProposals) #: uri)
    (failCustomNoArg @() #noSuchProposal)
    \info -> do
      choice <- new$ par #! #choice_index
      when (choice >= size (info #! #piChoices)) do
        failCustomNoArg @() #noSuchChoice
      when (info #! #piStartsAt > now) do
        failCustomNoArg @() #proposalNotYetActive
      when (info #! #piExpiresAt < now) do
        failCustomNoArg @() #proposalExpired
      updateStorageField @Storage #sVotes \votes -> do
        key <- new$ pair uri $ name #voter_address sender
        when (mem key votes) do
          failCustomNoArg @() #alreadyVoted
        pure $ votes +: (key, name #vote_choice choice)

[errorDocArg| "noSuchProposal" exception "No proposal for the given IPFS URI"|]
[errorDocArg| "noSuchChoice" exception "No choice with the given index available for the proposal"|]
[errorDocArg| "alreadyVoted" exception "The sender has already voted on this proposal"|]
[errorDocArg| "proposalNotYetActive" exception "The voting period for the proposal hasn't started yet"|]
[errorDocArg| "proposalExpired" exception "The voting period for the proposal has ended"|]
