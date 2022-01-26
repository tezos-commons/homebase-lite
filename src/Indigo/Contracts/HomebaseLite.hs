-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite
  ( Parameter(..)
  , Storage(..)
  , homebaseLiteCode
  , initialStorage
  ) where

import Indigo

import Indigo.Contracts.HomebaseLite.Impl
import Indigo.Contracts.HomebaseLite.Types

homebaseLiteCode :: ContractCode Parameter Storage
homebaseLiteCode = compileIndigoContract homebaseLiteContract
