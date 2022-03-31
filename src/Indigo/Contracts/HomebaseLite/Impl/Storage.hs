-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Storage
  ( initialStorage
  ) where

import Indigo hiding ((*))

import Morley.Util.Named (arg)

import Indigo.Contracts.HomebaseLite.Impl.Metadata
import Indigo.Contracts.HomebaseLite.Types

initialStorage
  :: ("admin" :! Address)
  -> ("expireTime" :! Seconds)
  -> ("voteDelay" :! Seconds)
  -> ("quorumThreshold" :! Natural)
  -> ("minimumBalance" :! Natural)
  -> ("fa2config" :! FA2Config)
  -> ("metadataConfig" :! MetadataConfig)
  -> Storage
initialStorage
  (arg #admin -> admin)
  (arg #expireTime -> expireTime)
  (arg #voteDelay -> voteDelay)
  (arg #quorumThreshold -> quorumThreshold)
  (arg #minimumBalance -> minimumBalance)
  (arg #fa2config -> fa2config)
  (arg #metadataConfig -> contractMetadataConfig)
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
    , sFA2Config = fa2config
    , sProposals = def
    , sVotes = def
    , sMetadata = metadataBigMap contractMetadataConfig
    }
