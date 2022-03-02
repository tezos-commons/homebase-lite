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

import Lorentz (CustomError, IsError, MText, MustHaveErrorArg, toAddress)

import Data.Default (def)
import Fmt (Buildable(..))
import GHC.TypeLits (symbolVal)

import qualified Indigo.Contracts.FA2Sample as FA2
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Morley.Tezos.Address
import Morley.Util.Label
import Morley.Util.Named (pattern (:!))
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

instance Buildable () where
  build () = "()"

instance Buildable URI where
  build (URI x) = "uri:" <> build x

deriving newtype instance Num Seconds

deployContract
  :: MonadCleveland caps m
  => m (Address, ContractHandle Parameter Storage ())
deployContract = deployContract' id

deployContract'
  :: MonadCleveland caps m
  => (Storage -> Storage)
  -> m (Address, ContractHandle Parameter Storage ())
deployContract' = deployContractWithConf Nothing

deployContractWithConf
  :: MonadCleveland caps m
  => Maybe FA2Config
  -> (Storage -> Storage)
  -> m (Address, ContractHandle Parameter Storage ())
deployContractWithConf fa2conf modStor = do
  admin <- newAddress "admin"
  let stor = modStor $ defaultStorage admin fa2conf
  contract <- originateSimple "HomebaseLite" stor lorentzContract
  pure (admin, contract)

defaultStorage :: Address -> Maybe FA2Config -> Storage
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
      { fa2Addr = [ta|KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9|]
      , fa2TokenId = TokenId 0
      }

deriving stock instance Eq Seconds
deriving stock instance Eq Configuration

instance Buildable Configuration where
  build = show

deriving stock instance Eq ProposalInfo

instance Buildable ProposalInfo where
  build = show

deployWithFA2 :: MonadCleveland caps m => m (Address, Address, ContractHandle Parameter Storage ())
deployWithFA2 = do
  holder <- newAddress "holder"
  let stor = FA2.mkStorage meta [((holder, FA2.TokenId 0), 100)] []
      meta = FA2.mkTokenMetadata "g" "governance" "0"
  fa2 <- originateSimple "FA2" stor (FA2.fa2Contract def)
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
