-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Indigo.Contracts.HomebaseLite.AsRPC
  ( ConfigurationRPC(..)
  , StorageRPC(..)
  , TokenIdRPC(..)
  , FA2ConfigRPC(..)
  , toRPC
  , toRPCProposalInfo
  ) where

import Lorentz (HasAnnotation)

import Morley.AsRPC (deriveRPCWithOptions, droRecursive)
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

deriveRPC "Seconds"
deriveRPC "Configuration"
deriveRPC "TokenId"
-- TODO [morley#922]: Just use deriveRPC here.
deriveRPCWithOptions "FA2Config" def{droRecursive=False}
deriveRPCWithOptions "Storage" def{droRecursive=False}
deriveRPC "ProposalInfo"

deriving stock instance Show SecondsRPC
deriving stock instance Eq SecondsRPC
deriving stock instance Show ConfigurationRPC
deriving stock instance Eq ConfigurationRPC
deriving stock instance Eq ProposalInfoRPC
deriving stock instance Show ProposalInfoRPC

toRPC :: Configuration -> ConfigurationRPC
toRPC (Configuration (Seconds a) (Seconds b) c d) =
  ConfigurationRPC (SecondsRPC a) (SecondsRPC b) c d

toRPCProposalInfo :: ProposalInfo -> ProposalInfoRPC
toRPCProposalInfo ProposalInfo {..} = ProposalInfoRPC
  { piLevelRPC = piLevel
  , piStartsAtRPC = piStartsAt
  , piExpiresAtRPC = piExpiresAt
  , piQuorumThresholdRPC = piQuorumThreshold
  , piChoicesRPC = piChoices
  }
