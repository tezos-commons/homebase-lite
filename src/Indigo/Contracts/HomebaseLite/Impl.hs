-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Impl
  ( homebaseLiteContract
  , initialStorage
  ) where

import Indigo hiding ((*))

import Morley.Util.Interpolate (itu)
import Morley.Util.Named

import Indigo.Contracts.HomebaseLite.Impl.Admin
import Indigo.Contracts.HomebaseLite.Impl.Vote
import Indigo.Contracts.HomebaseLite.Types

initialStorage
  :: ("admin" :! Address)
  -> ("expireTime" :! Seconds)
  -> ("voteDelay" :! Seconds)
  -> ("quorumThreshold" :! Natural)
  -> ("minimumBalance" :! Natural)
  -> Storage
initialStorage (N admin) (N expireTime) (N voteDelay) (N quorumThreshold) (N minimumBalance)
  = Storage
    { sAdmin = admin
    , sAdminCandidate = admin
    , sMaintainers = def
    , sConfiguration = Configuration
      { cExpireTime = expireTime
      , cVoteDelay = voteDelay
      , cQuorumThreshold = quorumThreshold
      , cMinimumBalance = minimumBalance
      }
    , sProposals = def
    , sVotes = def
    }

homebaseLiteContract :: IndigoContract Parameter Storage
homebaseLiteContract param = defContract $ docGroup "Homebase Lite" do
  contractGeneralDefault
  description [itu|
    Homebase Lite is an offchain, decentralized voting system. It aims to
    provide a very simple interface for communities to propose and vote on
    decisions that do not require binding actions, e.g. to request feedback,
    run polls, etc.

    It aims to be a very lightweight complement to Homebase.|]
  doc $ dStorage @Storage
  entryCaseSimple param
    ( #cSet_admin #= setAdmin
    , #cAccept_admin #= acceptAdmin
    , #cAdd_maintainers #= addMaintainers
    , #cRemove_maintainers #= removeMaintainers
    , #cConfigure #= configure
    , #cPropose #= propose
    , #cVote #= vote
    , #cVerify_min_balance #= verifyMinBalance
    )
