-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Impl.Contract
  ( lorentzContract
  ) where

import Indigo hiding ((*))
import Lorentz qualified as L

import Morley.Util.Interpolate (itu)

import Indigo.Contracts.HomebaseLite.Impl.Admin
import Indigo.Contracts.HomebaseLite.Impl.Vote
import Indigo.Contracts.HomebaseLite.Optimizer
import Indigo.Contracts.HomebaseLite.Types

indigoContract :: IndigoContract Parameter Storage
indigoContract param = defContract $ docGroup "Homebase Lite" do
  contractGeneralDefault
  description [itu|
    Homebase Lite is an offchain, decentralized voting system. It aims to
    provide a very simple interface for communities to propose and vote on
    decisions that do not require binding actions, e.g. to request feedback,
    run polls, etc.

    It aims to be a very lightweight complement to Homebase.|]
  doc $ dStorage @Storage
  entryCaseSimple param
    ( #cSet_admin #= setAdmin
    , #cAccept_admin #= acceptAdmin
    , #cAdd_maintainers #= addMaintainers
    , #cRemove_maintainers #= removeMaintainers
    , #cConfigure #= configure
    , #cPropose #= propose
    , #cVote #= vote
    , #cVerify_min_balance #= verifyMinBalance
    )

lorentzContract :: L.Contract Parameter Storage ()
lorentzContract = mkContractWith compilationOptions
  $ ContractCode . optimize . unContractCode
  $ compileIndigoContract indigoContract
  where
    -- Note: we run customized optimizer passes manually for more control of
    -- what and when we run, hence automatic optimization here is disabled.
    compilationOptions = defaultCompilationOptions { coOptimizerConf = Nothing }
