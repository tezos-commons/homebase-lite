-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module Indigo.Contracts.HomebaseLite.Impl.Admin
  ( setAdmin
  , acceptAdmin
  , addMaintainers
  , removeMaintainers
  , configure
  ) where

import Indigo hiding ((*))

import Morley.Util.Interpolate (itu)

setAdmin :: tp :~> Address => IndigoEntrypoint tp
setAdmin _ = do
  description [itu|
    Called by the current `admin` to transfer its role to someone else.
    Takes in input the address of the new `admin` candidate.
    Note: for security reasons the transfer isn't complete until the new admin candidate calls `accept_admin`.
    If there is already a candidate, another call to this entrypoint will replace it.
    If there is already a candidate, calling this with the `admin` address will invalidate the current candidate.
    |]
  pass


acceptAdmin :: tp :~> () => IndigoEntrypoint tp
acceptAdmin _ = do
  description [itu|
    Called by an `admin` candidate (see `set_admin`) to complete the transfer of the role.
    Takes no input.
    |]
  pass

[errorDoc| "senderIsNotAdminCandidate" exception "Fail when the sender is not the current admin
candidate"|]

addMaintainers :: IndigoEntrypoint tp
addMaintainers _ = do
  description [itu|
    Takes as input a list of addresses that will become `maintainer`s.
    All the given addresses receive the `maintainer` role.
    If a given address already has a `maintainer` role or is present in the list
    more than once, this call is a no-op.
    |]
  pass

removeMaintainers :: IndigoEntrypoint tp
removeMaintainers _ = do
  description [itu|
    Takes as input a list of addresses that will lose the `maintainer` role.
    All the given addresses lose the `maintainer` role.
    If a given addresses is not a `maintainer` this call is a no-op.
    |]
  pass

configure :: IndigoEntrypoint tp
configure _ = do
  description [itu|
    Takes as input new values for all the **configuration options**, these new values will replace the current ones and will affect future proposals.
    |]
  pass

[errorDoc| "senderIsNotMaintainer" exception "Fail when the sender does not have the `maintainer` role"|]
