-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Indigo.Contracts.HomebaseLite.Utils
  ( deployContract
  , deployContract'
  , deployContractWithConf
  ) where

import Lorentz (defaultContract)

import Fmt (Buildable(..))

import Morley.Tezos.Address
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
deployContract = deployContractWithConf FA2Config
  { fa2Addr = [ta|KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9|]
  , fa2TokenId = TokenId 0
  } id

deployContract'
  :: MonadCleveland caps m
  => (Storage -> Storage)
  -> m (Address, ContractHandle Parameter Storage ())
deployContract' = deployContractWithConf FA2Config
  { fa2Addr = [ta|KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9|]
  , fa2TokenId = TokenId 0
  }

deployContractWithConf
  :: MonadCleveland caps m
  => FA2Config
  -> (Storage -> Storage)
  -> m (Address, ContractHandle Parameter Storage ())
deployContractWithConf fa2conf modStor = do
  admin <- newAddress "admin"
  let stor = modStor $ initialStorage
        (#admin :! admin)
        (#expireTime :! 3600)
        (#voteDelay :! 600)
        (#quorumThreshold :! 1)
        (#minimumBalance :! 1)
        (#fa2config :! fa2conf)
  contract <- originateSimple "HomebaseLite" stor $ defaultContract homebaseLiteCode
  pure (admin, contract)
