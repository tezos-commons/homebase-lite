-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Indigo.Contracts.HomebaseLite.Utils
  ( deployContract
  , deployContract'
  , deployContractWithConf
  , defaultStorage
  , deployWithFA2
  , LabelEx(..)
  ) where

import Lorentz (CustomError, IsError, MText, MustHaveErrorArg)

import Fmt (Buildable(..))
import GHC.TypeLits (symbolVal)

import Indigo.Contracts.FA2Sample qualified as FA2
import Lorentz.Contracts.Spec.FA2Interface qualified as FA2
import Morley.Tezos.Address
import Morley.Util.Label
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

instance Buildable URI where
  build (URI x) = "uri:" <> build x

deriving newtype instance Num Seconds

deployContract
  :: MonadCleveland caps m
  => m (ImplicitAddress, ContractHandle Parameter Storage ())
deployContract = deployContract' id

deployContract'
  :: MonadCleveland caps m
  => (Storage -> Storage)
  -> m (ImplicitAddress, ContractHandle Parameter Storage ())
deployContract' = deployContractWithConf Nothing

deployContractWithConf
  :: MonadCleveland caps m
  => Maybe FA2Config
  -> (Storage -> Storage)
  -> m (ImplicitAddress, ContractHandle Parameter Storage ())
deployContractWithConf fa2conf modStor = do
  admin <- newAddress "admin"
  let stor = modStor $ defaultStorage (toL1Address admin) fa2conf
  contract <- originate "HomebaseLite" stor lorentzContract
  pure (admin, contract)

defaultStorage :: L1Address -> Maybe FA2Config -> Storage
defaultStorage admin fa2conf = initialStorage
  (#admin :! admin)
  (#expireTime :! 3600)
  (#voteDelay :! 600)
  (#quorumThreshold :! 1)
  (#minimumBalance :! 1)
  (#fa2config :! fromMaybe defaultFa2conf fa2conf)
  (#metadataConfig :! def)
  where
    defaultFa2conf = FA2Config
      { fa2Addr = toAddress [ta|KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9|]
      , fa2TokenId = TokenId 0
      }

deriving stock instance Eq Seconds
deriving stock instance Eq Configuration
deriving stock instance Eq ProposalInfo

deployWithFA2 :: MonadCleveland caps m => m (ImplicitAddress, ImplicitAddress, ContractHandle Parameter Storage ())
deployWithFA2 = do
  holder <- newAddress "holder"
  let stor = FA2.mkStorage meta [((toAddress holder, FA2.TokenId 0), 100)] []
      meta = FA2.mkTokenMetadata "g" "governance" "0"
  fa2 <- originate "FA2" stor (FA2.fa2Contract def)
  (admin, contract) <- deployContractWithConf (Just FA2Config
    { fa2Addr = toAddress fa2
    , fa2TokenId = TokenId 0
    }) id
  pure (holder, admin, contract)

data LabelEx
  = forall tag.
    (IsError (CustomError tag), MustHaveErrorArg tag MText)
  => LabelEx (Label tag)

instance Buildable LabelEx where
  build (LabelEx (Label :: Label tag)) = fromString $ symbolVal (Proxy @tag)
