-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Admin
  ( setAdmin
  , acceptAdmin
  , addMaintainers
  , removeMaintainers
  , configure
  ) where

import Indigo hiding (errorDoc)

import Morley.Util.Interpolate (itu)

import Indigo.Contracts.HomebaseLite.Impl.Utils
import Indigo.Contracts.HomebaseLite.Types

checkSenderIsAdmin :: HasStorage Storage => IndigoProcedure
checkSenderIsAdmin = defFunction do
  when (sender /= storage #! #sAdmin) do
    failCustomNoArg @() #senderIsNotAdmin_

setAdmin :: HomebaseLiteEntrypoint Address
setAdmin newAdmin = do
  description [itu|
    Called by the current `admin` to transfer its role to someone else.
    Takes in input the address of the new `admin` candidate.
    Note: for security reasons the transfer isn't complete until the new admin
    candidate calls `accept_admin`.
    If there is already a candidate, another call to this entrypoint will
    replace it.
    If there is already a candidate, calling this with the `admin` address will
    invalidate the current candidate.
    |]
  checkSenderIsAdmin
  setStorageField @Storage #sAdminCandidate newAdmin

acceptAdmin :: HomebaseLiteEntrypoint ()
acceptAdmin _ = do
  description [itu|
    Called by an `admin` candidate (see `set_admin`) to complete the transfer of
    the role. Takes no input.
    |]
  candidate <- getStorageField @Storage #sAdminCandidate
  when (sender /= candidate) do
    failCustomNoArg @() #senderIsNotAdminCandidate
  setStorageField @Storage #sAdmin candidate

-- TODO [morley#748] should use senderIsNotAdmin, but can't because Lorentz exprots it already
[errorDoc| "senderIsNotAdmin_" exception "The sender is not the current admin"|]
[errorDoc| "senderIsNotAdminCandidate" exception "The sender is not the current admin candidate"|]

addMaintainers :: HomebaseLiteEntrypoint [Address]
addMaintainers newMaintainers = do
  description [itu|
    Takes as input a list of addresses that will become `maintainer`s.
    All the given addresses receive the `maintainer` role.
    If a given address already has a `maintainer` role or is present in the list
    more than once, this call is a no-op.
    |]
  checkSenderIsAdmin
  modifyMaintainers (\l r -> l +: (r, ())) newMaintainers

removeMaintainers :: HomebaseLiteEntrypoint [Address]
removeMaintainers maintainersToRemove = do
  description [itu|
    Takes as input a list of addresses that will lose the `maintainer` role.
    All the given addresses lose the `maintainer` role.
    If a given addresses is not a `maintainer` this call is a no-op.
    |]
  checkSenderIsAdmin
  modifyMaintainers (-:) maintainersToRemove

modifyMaintainers
  :: (HasStorage Storage, tp :~> [Address])
  => (Var (BigMap Address ()) -> Var Address -> Expr (BigMap Address ()))
  -> tp
  -> IndigoProcedure
modifyMaintainers modf list = defFunction do
  updateStorageField @Storage #sMaintainers \maints -> do
    forEach list \maint -> do
      maints =: maints `modf` maint
    pure maints

configure :: HomebaseLiteEntrypoint Configuration
configure newConfig = do
  description [itu|
    Takes as input new values for all the **configuration options**, these new
    values will replace the current ones and will affect future proposals.
    |]
  checkSenderIsMaintainer
  setStorageField @Storage #sConfiguration newConfig

checkSenderIsMaintainer :: HasStorage Storage => IndigoProcedure
checkSenderIsMaintainer =
  unless (mem sender $ storage #! #sMaintainers) do
    failCustomNoArg @() #senderIsNotMaintainer

[errorDoc| "senderIsNotMaintainer" exception "The sender does not have the `maintainer` role"|]
