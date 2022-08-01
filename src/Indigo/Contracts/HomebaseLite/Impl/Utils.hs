-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Impl.Utils
  ( HomebaseLiteEntrypoint
  , storage
  ) where

import Indigo (HasStorage, IndigoEntrypoint, Var, storageVar, type (:~>))

import Indigo.Contracts.HomebaseLite.Types

type HomebaseLiteEntrypoint typ = forall tp. (HasStorage Storage, tp :~> typ) => IndigoEntrypoint tp

storage :: HasStorage Storage => Var Storage
storage = storageVar
