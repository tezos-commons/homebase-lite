-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE OverloadedLists #-}

module Main
  ( main
  ) where

import Lorentz
  (Contract, DGitRevision(..), GitRepoSettings(..), NiceStorage, attachDocCommons, buildMarkdownDoc,
  defaultContract, docItemToMarkdown, mkDGitRevision, niceStorageEvi, printLorentzValue,
  toMichelsonContract, toVal, zeroMutez)

import Data.Aeson.Encode.Pretty (encodePretty, encodePrettyToTextBuilder)
import qualified Data.ByteString.Lazy.Char8 as BS (putStrLn)
import Data.Char (isUpper, toLower)
import Data.Constraint ((\\))
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy.IO.Utf8 as Utf8 (writeFile)
import Fmt (pretty)
import GHC.TypeLits (symbolVal)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import System.Environment (withProgName)

import Morley.CLI (addressOption, onelineOption)
import Morley.Client
  (AddressOrAlias(AddressResolved), clientConfigParser, lOriginateContract, mkAliasHint,
  mkMorleyClientEnv, runMorleyClientM)
import Morley.Micheline (Expression, toExpression)
import Morley.Michelson.Analyzer (analyze)
import Morley.Michelson.Printer (printTypedContract)
import Morley.Michelson.Typed (cCode)
import Morley.Util.Markdown (HeaderLevel(..))
import Morley.Util.Named (Name(..), pattern (:!), type (:!))

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

fa2ConfigParser :: Opt.Parser ("fa2config" :! FA2Config)
fa2ConfigParser = fmap (#fa2config :!) $ FA2Config <$> addrParser <*> tokenIdParser
  where
    addrParser = addressOption Nothing
      (#name :! "fa2-address")
      (#help :! "FA2 contract address")
    tokenIdParser = fmap TokenId . Opt.option (Opt.maybeReader readMaybe) $
      Opt.long "fa2-token-id" <>
      Opt.metavar "NATURAL" <>
      Opt.value 0 <>
      Opt.help "FA2 token type identifier" <>
      Opt.showDefault

usageDoc :: Doc
usageDoc = mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  homebase-lite print --help", linebreak
   ]

contract :: Contract Parameter Storage ()
contract = defaultContract $ homebaseLiteCode

storageParser :: Opt.Parser Storage
storageParser = initialStorage
  <$> adminParser
  <*> expireTimeParser
  <*> voteDelayParser
  <*> quorumThresholdParser
  <*> minimumBalanceParser
  <*> fa2ConfigParser
  where
    adminParser = (#admin :!) <$> addressOption Nothing
      (#name :! "admin")
      (#help :! "Initial admin address")
    toLong (Name :: Name s) = concat $ flip map (symbolVal $ Proxy @s) \case
      x | isUpper x -> ['-', toLower x]
        | otherwise -> [x]
    naturalParser ctor label deflt help =
      fmap ((label :!) . ctor) .
        Opt.option (Opt.maybeReader readMaybe) $
          Opt.long (toLong label) <>
          Opt.metavar "NATURAL" <>
          Opt.value deflt <>
          Opt.help help <>
          Opt.showDefault
    expireTimeParser =naturalParser Seconds #expireTime (7 * 24 * 3600)
      "After how much time a proposal can no longer be voted on (in seconds)"
    voteDelayParser =naturalParser Seconds #voteDelay 0
      "After how much time votes can be cast on a proposal (in seconds)"
    quorumThresholdParser = naturalParser id #quorumThreshold 0
      "The minimum amount of votes to be cast on a proposal for it to be successful, \
      \expressed as a constant number to reach"
    minimumBalanceParser = naturalParser id #minimumBalance 0
      "The minimum amount of governance tokens needed to submit a new proposal"

argParser :: Opt.Parser (IO ())
argParser = Opt.subparser $ mconcat $
  [ printSubCmd
  , documentSubCmd
  , analyzerSubCmd
  , storageSubCmd
  , originateSubCmd
  , versionSubCmd
  ]
  where
    contractName = "homebase-lite"
    compiledContract = toMichelsonContract $ contract

    printSubCmd = mkCommandParser "print" "Dump a contract in form of Michelson code" $
      (<*> michelineOption) $
      (<*> onelineOption) $
      outputOptions <&> \mOutput forceOneLine useMicheline ->
        writeFunc (contractName <> ".tz") mOutput $
          if useMicheline
          then toLazyText $ encodePrettyToTextBuilder $ toExpression compiledContract
          else printTypedContract forceOneLine compiledContract

    documentSubCmd = mkCommandParser "document" "Dump contract documentation in Markdown" $
      outputOptions <&> \mOutput -> writeFunc (contractName <> ".md") mOutput $
        buildMarkdownDoc $ attachDocCommons gitRev contract

    analyzerSubCmd =
      mkCommandParser "analyze" "Analyze the contract and prints statistics about it." $
        pure $ putTextLn $ pretty $ analyze $ cCode compiledContract

    storageSubCmd = mkCommandParser "storage" "Print initial storage for the contract" $
      (<*> michelineOption) $
      storageParser <&> \storage useMicheline ->
        if useMicheline
        then BS.putStrLn $ encodePretty $ toExpressionHelper storage
        else putStrLn $ printLorentzValue True storage

    originateSubCmd = mkCommandParser "originate" "Originate the contract. \
      \Note that the origination is run as the contract admin." $
      (<*> clientConfigParser (pure Nothing)) $
      (<*> storageParser) $
      nameOption <&> \name storage@Storage{..} cconf -> do
        putStrLn $ "Originating " <> name <> "..."
        env <- mkMorleyClientEnv cconf
        (_, addr) <- runMorleyClientM env $ do
          lOriginateContract False (mkAliasHint name) (AddressResolved sAdmin) zeroMutez
            contract storage Nothing
        putStrLn $ "Originated " <> name <> " as " <> pretty addr

    versionSubCmd = mkCommandParser "version" "Show binary revision number" $
      pure $ putStrLn $ toLazyText $ docItemToMarkdown (HeaderLevel 0) gitRev

    nameOption = Opt.strOption $ mconcat
      [ Opt.short 'n'
      , Opt.long "name"
      , Opt.metavar "IDENTIFIER"
      , Opt.help "Name of a contract recorded into tezos-client."
      , Opt.value (fromString contractName)
      , Opt.showDefault
      ]

    outputOptions = optional . Opt.strOption $ mconcat
      [ Opt.short 'o'
      , Opt.long "output"
      , Opt.metavar "FILEPATH"
      , Opt.help $
        "File to use as output. If not specified, the file name " <>
        "will be constructed from the contract name. " <>
        "Pass - to use stdout."
      ]

    michelineOption = Opt.switch (
      Opt.long "micheline" <>
      Opt.help "Print using low-level Micheline representation")

    mkCommandParser commandName desc parser =
      Opt.command commandName $
      Opt.info (Opt.helper <*> parser) $
      Opt.progDesc desc

    writeFunc defName = \case
      Nothing -> Utf8.writeFile defName
      Just "-" -> putStrLn
      Just output -> Utf8.writeFile output

    toExpressionHelper :: forall st'. NiceStorage st' => st' -> Expression
    toExpressionHelper = toExpression . toVal \\ niceStorageEvi @st'

gitRev :: DGitRevision
gitRev = $mkDGitRevision $ GitRepoSettings ("https://github.com/tezos-commons/homebase-lite/commit/" <>)

main :: IO ()
main = withUtf8 $ withProgName "homebase-lite" $ do
  join $ Opt.execParser $ Opt.info (
    argParser <**> Opt.helper) $
    mconcat
    [ Opt.fullDesc
    , Opt.progDesc "This executable provides commands for development and interaction with the project."
    , Opt.header "Homebase-Lite"
    , Opt.footerDoc $ Just usageDoc
    ]
  `catchAny` (die . displayException)
