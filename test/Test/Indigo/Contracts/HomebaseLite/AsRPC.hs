-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.AsRPC
  ( ConfigurationRPC(..)
  , StorageRPC(..)
  , SecondsRPC(..)
  ) where

import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

deriveRPC "Seconds"
deriveRPC "Configuration"
deriveRPC "Storage"

deriving stock instance Show SecondsRPC
deriving stock instance Eq SecondsRPC
deriving stock instance Show ConfigurationRPC
deriving stock instance Eq ConfigurationRPC
