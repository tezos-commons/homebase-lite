-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE OverloadedLists #-}

module Indigo.Contracts.HomebaseLite.Impl.Metadata
  ( metadataBigMap
  , versionString
  , gitRev
  ) where

import Indigo hiding (cast, description, name, (<>))
import Lorentz qualified as L

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BS
import Data.Text.Internal.Builder (toLazyText)
import Data.Typeable (cast)

import Lorentz.Contracts.Spec.TZIP16Interface qualified as TZ16
import Morley.Metadata (compileViewCodeTH, mkSimpleMichelsonStorageView)
import Morley.Micheline (toExpression)
import Morley.Michelson.Text (mkMText)
import Morley.Util.Markdown (HeaderLevel(..))

import Indigo.Contracts.HomebaseLite.Impl.Contract
import Indigo.Contracts.HomebaseLite.Impl.Metadata.Views
import Indigo.Contracts.HomebaseLite.Types

metadataBigMap :: MetadataConfig -> TZ16.MetadataMap
metadataBigMap conf = TZ16.metadataURI (TZ16.tezosStorageUri TZ16.selfHost metadataKey)
  <> [(metadataKey, BS.toStrict $ encode $ metadataJSON conf)]
  where metadataKey = [mt|metadata|]

metadataJSON :: MetadataConfig -> TZ16.Metadata (ToT Storage)
metadataJSON MetadataConfig{..} = mconcat
  [ TZ16.name mcName
  , TZ16.description mcDescription
  , TZ16.authors [ TZ16.author "Serokell" "https://serokell.io"
                 , TZ16.author "Tezos Commons" "https://tezoscommons.org/" ]
  , TZ16.version versionString
  , TZ16.license $ TZ16.License "MIT" Nothing
  , TZ16.homepage "https://github.com/tezos-commons/homebase-lite"
  , TZ16.errors collectContractErrors
  , TZ16.views
      [ currentConfigView
      , proposalInfoView
      ]
  , TZ16.interfaces [TZ16.tzip 16]
  ]

collectContractErrors :: [TZ16.Error]
collectContractErrors =
  let d = L.buildDoc $ L.finalizedAsIs lorentzContract
  in catMaybes $ toList (cdDefinitionsSet d) <&> \(SomeDocDefinitionItem item) ->
    (cast item :: Maybe DError) <&> \(DError (Proxy :: Proxy e)) ->
      TZ16.EStatic TZ16.StaticError
        { seError = toExpression . toVal . unsafe . mkMText $ errorDocName @e
        , seExpansion = toExpression
          . toVal . unsafe . mkMText . toText . toLazyText $ errorDocMdCause @e
        , seLanguages = ["en-US"]
        }

currentConfigView :: TZ16.View (ToT Storage)
currentConfigView = TZ16.View
  { vName = "currentConfig"
  , vDescription = Nothing
  , vPure = Nothing
  , vImplementations = one $ TZ16.VIMichelsonStorageView $
      mkSimpleMichelsonStorageView $ $$(compileViewCodeTH currentConfigCode)
  }

proposalInfoView :: TZ16.View (ToT Storage)
proposalInfoView = TZ16.View
  { vName = "proposalInfo"
  , vDescription = Nothing
  , vPure = Nothing
  , vImplementations = one $ TZ16.VIMichelsonStorageView $
      mkSimpleMichelsonStorageView $ $$(compileViewCodeTH proposalInfoCode)
  }

versionString :: Text
versionString = toText $ toLazyText $ docItemToMarkdown (HeaderLevel 0) gitRev

gitRev :: DGitRevision
gitRev = $mkDGitRevision $ GitRepoSettings ("https://github.com/tezos-commons/homebase-lite/commit/" <>)
