-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Types.Self
  ( Parameter(..)
  , Storage(..)
  , Configuration(..)
  , URI(..)
  , Seconds(..)
  , ProposalInfo(..)
  , FA2Config(..)
  , MetadataConfig(..)
  ) where

import Indigo hiding ((<>))

import Lorentz.Contracts.Spec.FA2Interface (BalanceResponseItem, TokenId)
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)
import Morley.Michelson.Typed.Haskell.Doc (FieldCamelCase)

data Parameter
  = Set_admin Address
  | Accept_admin ()
  | Add_maintainers [Address]
  | Remove_maintainers [Address]
  | Configure Configuration
  | Propose ("proposal_uri" :! URI, "choices" :! [MText])
  | Vote ("proposal_uri" :! URI, "choice_index" :! Natural)
  | Verify_min_balance [BalanceResponseItem]
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

[entrypointDoc| Parameter plain |]

newtype Seconds = Seconds {unSeconds :: Natural}
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

[typeDoc| Seconds "A natural representing a number of seconds."|]

data Configuration = Configuration
  { cExpireTime :: Seconds
  , cVoteDelay :: Seconds
  , cQuorumThreshold :: Natural
  , cMinimumBalance :: Natural
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)
  deriving TypeHasFieldNamingStrategy via FieldCamelCase

[typeDoc| Configuration "Options defining the behaviour and life-cycle of proposals."|]

data ProposalInfo = ProposalInfo
  { piLevel :: Natural
  , piStartsAt :: Timestamp
  , piExpiresAt :: Timestamp
  , piQuorumThreshold :: Natural
  , piChoices :: [MText]
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)
  deriving TypeHasFieldNamingStrategy via FieldCamelCase

[typeDoc| ProposalInfo "Information defining a proposal."|]

newtype URI = URI MText
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (IsoValue, HasAnnotation)

[typeDoc| URI "Text representing IPFS URI for a proposal."|]

data Storage = Storage
  { sAdmin :: Address
  , sAdminCandidate :: Address
  , sMaintainers :: BigMap Address ()
  , sConfiguration :: Configuration
  , sFA2Config :: FA2Config
  , sProposals :: BigMap ("proposal_uri" :! URI) ProposalInfo
  , sVotes :: BigMap ("proposal_uri" :! URI, "voter_address" :! Address)
      ("vote_choice" :! Natural)
  , sMetadata :: MetadataMap
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)
  deriving TypeHasFieldNamingStrategy via FieldCamelCase

[typeDoc| Storage "Contract storage."|]

data FA2Config = FA2Config
  { fa2Addr :: Address
  , fa2TokenId :: TokenId
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue, HasAnnotation)

instance TypeHasDoc FA2Config where
  typeDocMdDescription = "Parameters defining the governance token contract and type"
  typeDocHaskellRep = homomorphicTypeDocHaskellRep

data MetadataConfig = MetadataConfig
  { mcName :: Text
  , mcDescription :: Text
  }

instance Default MetadataConfig where
  def = MetadataConfig "Homebase-Lite" "Offchain, decentralized voting system."
