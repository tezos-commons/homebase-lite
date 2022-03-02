-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Indigo.Contracts.HomebaseLite.Metadata.Data
  ( test_metadata_simple
  ) where

import Fmt (Buildable(..), pretty)
import Test.Tasty (TestTree)

import Lorentz (errorTagToMText, toVal)
import Lorentz.Contracts.Spec.TZIP16Interface
import Morley.Micheline (toExpression)
import Test.Cleveland
import Test.Morley.Metadata

import Test.Indigo.Contracts.HomebaseLite.Utils

test_metadata_simple :: TestTree
test_metadata_simple =
  testScenario "simple metadata fields are correct" $ scenario do
    (_, contract) <- deployContract
    meta <- getMetadata contract
    evalRight pretty (getName meta) @@== Just "Homebase-Lite"
    evalRight pretty (getDescription meta) @@== Just "Offchain, decentralized voting system."
    evalRight pretty (getLicense meta) @@== Just (License "MIT" Nothing)
    evalRight pretty (getHomepage meta) @@== Just "https://github.com/tezos-commons/homebase-lite"
    declaredErrors <- evalRight pretty $ getErrors meta
    for_ knownErrors \err ->
      assert (hasError err declaredErrors) $ pretty err <> " defined in errors"
  where
    knownErrors =
      [ LabelEx #senderIsNotAdmin_
      , LabelEx #senderIsNotAdminCandidate
      , LabelEx #senderIsNotMaintainer
      , LabelEx #notEnoughTokens
      , LabelEx #duplicateProposal
      , LabelEx #emptyChoices
      , LabelEx #noFA2Contract
      , LabelEx #noSuchProposal
      , LabelEx #noSuchChoice
      , LabelEx #alreadyVoted
      , LabelEx #proposalNotYetActive
      , LabelEx #proposalExpired
      ]
    hasError :: LabelEx -> [Error] -> Bool
    hasError k = isJust . find (errHasKey k)
    errHasKey :: LabelEx -> Error -> Bool
    errHasKey (LabelEx k) (EStatic StaticError{..}) =
      seError == toExpression (toVal (errorTagToMText k))
    errHasKey _ _ = False

instance Buildable License where
  build = show
