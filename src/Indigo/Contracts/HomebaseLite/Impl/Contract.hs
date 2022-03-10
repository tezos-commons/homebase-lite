-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Contract
  ( lorentzContract
  ) where

import Indigo hiding ((*))
import qualified Lorentz as L

import Morley.Util.Interpolate (itu)

import Indigo.Contracts.HomebaseLite.Impl.Admin
import Indigo.Contracts.HomebaseLite.Impl.Vote
import Indigo.Contracts.HomebaseLite.Optimizer
import Indigo.Contracts.HomebaseLite.Types
import Indigo.Contracts.HomebaseLite.Impl.Utils (HomebaseLiteEntrypoint)

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
  entryCase (Proxy @PlainEntrypointsKind) param
    ( #cAdmin #= adminEps
    , #cVoting #= votingEps
    )


adminEps :: HomebaseLiteEntrypoint AdminParameter
adminEps param = entryCaseSimple param
  ( #cSet_admin #= setAdmin
  , #cAccept_admin #= acceptAdmin
  , #cAdd_maintainers #= addMaintainers
  , #cRemove_maintainers #= removeMaintainers
  , #cConfigure #= configure
  )

votingEps :: HasSideEffects => HomebaseLiteEntrypoint VotingParameter
votingEps param = entryCaseSimple param
  ( #cPropose #= propose
  , #cVote #= vote
  , #cVerify_min_balance #= verifyMinBalance
  )


lorentzContract :: L.Contract Parameter Storage ()
lorentzContract = defaultContract $ optimize $ compileIndigoContract indigoContract
