-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Types.Self
  ( Parameter(..)
  , AdminParameter(..)
  , VotingParameter(..)
  , Storage(..)
  , Configuration(..)
  , URI(..)
  , Seconds(..)
  , ProposalInfo(..)
  , FA2Config(..)
  , MetadataConfig(..)
  ) where

import Indigo hiding ((<>))

import Data.Char (isLower, isUpper, toLower)
import qualified Data.Text as T

import Lorentz.Annotation (AnnOptions(..), annOptions)
import Lorentz.Contracts.Spec.FA2Interface (BalanceResponseItem, TokenId)
import Lorentz.Contracts.Spec.TZIP16Interface (MetadataMap)

data Parameter
  = Admin AdminParameter
  | Voting VotingParameter
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

[entrypointDoc| Parameter recursive |]

data AdminParameter
  = Set_admin Address
  | Accept_admin ()
  | Add_maintainers [Address]
  | Remove_maintainers [Address]
  | Configure Configuration
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

[entrypointDoc| AdminParameter plain |]
[typeDoc| AdminParameter "AdminParameter"|]

data VotingParameter
  = Propose ("proposal_uri" :! URI, "choices" :! [MText])
  | Vote ("proposal_uri" :! URI, "choice_index" :! Natural)
  | Verify_min_balance [BalanceResponseItem]
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

[entrypointDoc| VotingParameter plain |]
[typeDoc| VotingParameter "VotingParameter"|]

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
  deriving anyclass (IsoValue)

instance HasAnnotation Configuration where
  annOptions = myAnnOptions

[typeDoc| Configuration "Options defining the behaviour and life-cycle of proposals."|]

data ProposalInfo = ProposalInfo
  { piLevel :: Natural
  , piStartsAt :: Timestamp
  , piExpiresAt :: Timestamp
  , piQuorumThreshold :: Natural
  , piChoices :: [MText]
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

instance HasAnnotation ProposalInfo where
  annOptions = myAnnOptions

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
  , sMetadata :: MetadataMap BigMap
  }
  deriving stock (Generic, Show)
  deriving anyclass (IsoValue)

instance HasAnnotation Storage where
  annOptions = myAnnOptions

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

myAnnOptions :: AnnOptions
myAnnOptions = AnnOptions stripPrefix

-- this is cloned from "Morley.Michelson.Typed.Haskell.Doc"
-- [TODO: morley#759]: Remove this when 'dropPrefixThen' behaves the same, or this whole
-- manual tweaking becomes unnecessary.
stripPrefix :: Text -> Text
stripPrefix fieldName =
  case T.uncons $ T.dropWhile isLower fieldName of
    Nothing -> error $ "Field '" <> fieldName <> "' has no prefix"
    Just (c, cs) ->
      -- For fields like @ciUSPosition@ we should not lead the first letter
      -- to lower case like @uSPosition@.
      let isAbbreviation = case T.uncons cs of
            Just (c2, _)
              | isUpper c2 -> True
              | otherwise -> False
            Nothing -> False
      in T.cons (if isAbbreviation then c else toLower c) cs
