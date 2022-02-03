-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Storage
  ( initialStorage
  ) where

import Indigo hiding ((*))

import Morley.Util.Named (pattern N)

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
initialStorage (N admin) (N expireTime) (N voteDelay) (N quorumThreshold) (N minimumBalance)
  (N fa2config) (N contractMetadataConfig)
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
