-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE RebindableSyntax #-}

module Indigo.Contracts.HomebaseLite.Impl.Metadata.Views
  ( currentConfigCode
  , proposalInfoCode
  ) where

import Lorentz hiding (ViewCode)

import Morley.Metadata (ViewCode(..))

import Indigo.Contracts.HomebaseLite.Impl.Vote ()
import Indigo.Contracts.HomebaseLite.Optimizer
import Indigo.Contracts.HomebaseLite.Types

currentConfigCode :: ViewCode Storage Configuration
currentConfigCode = WithoutParam $ optimize $ toField #sConfiguration

proposalInfoCode :: ViewCode Storage ProposalInfo
proposalInfoCode = WithParam @("proposal_uri" :! URI) $ optimize do
  dip (toField #sProposals)
  get
  ifSome nop $ failCustomNoArg #noSuchProposal
