-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Types.FA2
  ( BalanceRequestItem(..)
  , BalanceResponseItem(..)
  , FA2Parameter
  , TokenId(..)
  ) where

import Lorentz.Contracts.Spec.FA2Interface
import Lorentz.Contracts.Spec.FA2Interface.ParameterInstances ()

type FA2Parameter = Parameter
