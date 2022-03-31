-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Impl.Utils
  ( HomebaseLiteEntrypoint
  , storage
  , errorDoc
  ) where

import Indigo
  (CustomErrorHasDoc(..), ErrorArg, ErrorClass, HasStorage, IndigoEntrypoint, NoErrorArg, Var,
  storageVar, type (:~>))

import Data.Text (stripPrefix, stripSuffix)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH (lift)

import Indigo.Contracts.HomebaseLite.Types

type HomebaseLiteEntrypoint typ = forall tp. (HasStorage Storage, tp :~> typ) => IndigoEntrypoint tp

storage :: HasStorage Storage => Var Storage
storage = storageVar

-- TODO [morley#747] remove this
-- | QuasiQuote that helps generating @CustomErrorHasDoc@ instance.
--
-- Usage:
--
-- @
-- [errorDoc| \<error-name> \<error-type> \<error-description> |]
-- [errorDoc| "errorName" exception "Error description" |]
-- @
--
-- See this [tutorial](https://indigo-lang.gitlab.io/contract-docs/) which
-- includes this quasiquote.
--
errorDoc :: QuasiQuoter
errorDoc = QuasiQuoter
  { quoteExp  = const $ fail $ toString qqName <> "cannot be used as expression"
  , quotePat  = const $ fail $ toString qqName <> "cannot be used as pattern"
  , quoteType = const $ fail $ toString qqName <> "cannot be used as type"
  , quoteDec  = go
  }
  where
    qqName = "errorDoc"

    errMsg i = unlines
      [ "Invalid arguments."
      , "      Expected arguments to be in the format of:"
      , "        - [" <> qqName <> "| <error-name> <error-type> <error-description> |]"
      , "      Examples:"
      , "        - [" <> qqName <> "| \"errorName\" exception \"Error description\" |]"
      , "        - [" <> qqName <> "| \"myError\" bad-argument \"An error happened\" |]"
      , "      But instead got: " <> unwords i
      ]

    go :: String -> Q [Dec]
    go input =
      let
        extract :: [Text] -> Either Text (Text, ExpQ, Text)
        extract i = case i of
            errorName:errorClassString:errorDesc ->
              case readMaybe @ErrorClass (toString errorClassString) of
                Just errorClass -> Right
                  ( stripQuote $ errorName
                  , TH.lift errorClass
                  , stripQuote . unwords $ errorDesc
                  )
                Nothing -> Left . errMsg $ i
            _ -> Left . errMsg $ i
      in case  extract $ words $ toText input of
            Right (errorName, errorClassVal, errorDesc) ->
              [d|
                type instance ErrorArg $(litT . strTyLit $ toString $ errorName) = NoErrorArg
                instance CustomErrorHasDoc $(litT . strTyLit $ toString $ errorName) where
                  customErrClass = $(errorClassVal)
                  customErrDocMdCause = $(litE $ stringL $ toString $ errorDesc)
              |]
            Left err -> fail $ toString err

stripQuote :: Text -> Text
stripQuote txt =
  let
    h = stripPrefix "\"" txt ?: txt
    g = stripSuffix "\"" h ?: h
  in g
