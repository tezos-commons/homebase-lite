-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module Indigo.Contracts.HomebaseLite.Impl.Vote
  ( propose
  , vote
  , verifyMinBalance
  ) where

import Indigo hiding ((*))

import Morley.Util.Interpolate (itu)

propose :: IndigoEntrypoint tp
propose _ = do
  description [itu|
    Called by a `participant` to submit a new proposal.
    Takes as input an IPFS URI and a list of strings for the `n`  available choices.
    A new proposal for the given IPFS will be initialized (with a `submitted` status).
    |]
  pass

verifyMinBalance :: IndigoEntrypoint tp
verifyMinBalance _ = do
  description [itu|
    Callback for the `balance_of` CPS view of FA2,
    used to check the balance of a `participant` that used the `propose` entrypoint.
    Takes as input the list of tuples containing the `owner` , `token_id` and `balance` .
    |]
  pass

[errorDoc| "notEnoughTokens" exception "Some of the `balance`s in the list are
less than `minimum_balance`"|]
[errorDoc| "duplicateProposal" exception "Fail if a proposal for the same IPFS URI was already
submitted before"|]
[errorDoc| "emptyChoices" bad-argument "Fail if the number of choices `n` is `0`"|]

vote :: IndigoEntrypoint tp
vote _ = do
  description [itu|
    Called by a `participant` to cast a vote on a proposal.
    Takes as input the proposal's IPFS URI and the choice made `x` .
    |]
  pass

[errorDoc| "noSuchProposal" exception "Fail if there is no proposal for the given IPFS URI"|]
[errorDoc| "noSuchChoice" exception "Fail if there is no choice `x` available for the given proposal, aka if `x > n`"|]
[errorDoc| "alreadyVoted" exception "Fail if the sender already voted on this proposal before"|]
[errorDoc| "proposalNotYetActive" exception "Fail if less than `vote_delay` has passed since the proposal submission, aka the proposal is still in a `sumbitted` status"|]
[errorDoc| "proposalExpired" exception "Fail if more than `expire_time` has passed since the proposal submission, aka the proposal has been completed (either as `expired` or `passed`)"|]
