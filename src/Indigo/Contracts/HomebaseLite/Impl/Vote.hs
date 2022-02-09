-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Vote
  ( propose
  , vote
  , verifyMinBalance
  ) where

import Indigo hiding (errorDoc, (*))

import Morley.Util.Interpolate (itu)

import Indigo.Contracts.HomebaseLite.Impl.Utils
import Indigo.Contracts.HomebaseLite.Types

propose :: HasSideEffects => HomebaseLiteEntrypoint ("proposal_uri" :! URI, "choices" :! [MText])
propose par = do
  description [itu|
    Called by a `participant` to submit a new proposal.
    Takes as input an IPFS URI and a list of strings for the `n`  available choices.
    A new proposal for the given IPFS will be initialized (with a `submitted` status).
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
    used to check the balance of a `participant` that used the `propose` entrypoint.
    Takes as input the list of tuples containing the `owner` , `token_id` and `balance` .
    |]
  forEach bris \bri -> do
    when (bri #! #briBalance < storage #! #sConfiguration #! #cMinimumBalance) do
      failCustomNoArg @() #notEnoughTokens

[errorDoc| "notEnoughTokens" exception "Some of the `balance`s in the list are
less than `minimum_balance`"|]
[errorDoc| "duplicateProposal" exception "Fail if a proposal for the same IPFS
URI was already submitted before"|]
[errorDoc| "emptyChoices" bad-argument "The number of choices `n` is `0`."|]
[errorDoc| "noFA2Contract" contract-internal "Configured FA2 contract not found."|]

vote :: HomebaseLiteEntrypoint ("proposal_uri" :! URI, "choice_index" :! Natural)
vote par = do
  description [itu|
    Called by a `participant` to cast a vote on a proposal.
    Takes as input the proposal's IPFS URI and the choice made `x` .
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

[errorDoc| "noSuchProposal" exception "There is no proposal for the given IPFS URI"|]
[errorDoc| "noSuchChoice" exception "There is no choice `x` available for the given proposal,
aka if `x > n`"|]
[errorDoc| "alreadyVoted" exception "The sender already voted on this proposal before"|]
[errorDoc| "proposalNotYetActive" exception "Less than `vote_delay` has passed since the proposal
submission, aka the proposal is still in a `sumbitted` status"|]
[errorDoc| "proposalExpired" exception "More than `expire_time` has passed since the proposal
submission, aka the proposal has been completed (either as `expired` or `passed`)"|]
