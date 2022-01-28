-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.AsRPC
  ( ConfigurationRPC(..)
  , StorageRPC(..)
  , TokenIdRPC(..)
  , FA2ConfigRPC(..)
  , toRPC
  ) where

import Fmt (Buildable(..))

import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

deriveRPC "Seconds"
deriveRPC "Configuration"
deriveRPC "TokenId"
deriveRPC "FA2Config"
deriveRPC "Storage"

deriving stock instance Show SecondsRPC
deriving stock instance Eq SecondsRPC
deriving stock instance Show ConfigurationRPC
deriving stock instance Eq ConfigurationRPC

instance Buildable ConfigurationRPC where
  build = show

toRPC :: Configuration -> ConfigurationRPC
toRPC (Configuration (Seconds a) (Seconds b) c d) =
  ConfigurationRPC (SecondsRPC a) (SecondsRPC b) c d
