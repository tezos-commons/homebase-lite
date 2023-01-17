-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import Lorentz
  (attachDocCommons, buildMarkdownDoc, def, printLorentzValue, toAddress, toMichelsonContract,
  toVal, zeroMutez)

import Data.Aeson.Encode.Pretty (encodePretty, encodePrettyToTextBuilder)
import Data.ByteString.Lazy.Char8 qualified as BS (putStrLn)
import Data.Char (isUpper, toLower)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.IO.Utf8 qualified as Utf8 (writeFile)
import Fmt (nameF, pretty)
import GHC.TypeLits (symbolVal)
import Main.Utf8 (withUtf8)
import Options.Applicative qualified as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import System.Environment (withProgName)

import Morley.CLI (addressOption, onelineOption)
import Morley.Client
  (AliasBehavior(..), clientConfigParser, lOriginateContract, mkMorleyClientEnv, runMorleyClientM)
import Morley.Client.RPC.Types
import Morley.Micheline (toExpression)
import Morley.Michelson.Analyzer (analyze)
import Morley.Michelson.Printer (printTypedContract)
import Morley.Michelson.Typed (cCode, unContractCode)
import Morley.Tezos.Address
import Morley.Tezos.Address.Alias (AddressOrAlias(..), Alias(..))
import Morley.Tezos.Address.Kinds
import Morley.Util.CLI (HasCLReader(..), mkCLOptionParser)
import Morley.Util.Named (Name, pattern (:!), type (:!))
import Morley.Util.Typeable

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Impl.Metadata (gitRev, versionString)
import Indigo.Contracts.HomebaseLite.Types

fa2ConfigParser :: Opt.Parser ("fa2config" :! FA2Config)
fa2ConfigParser = fmap (#fa2config :!) $ FA2Config <$> (toAddress <$> addrParser) <*> tokenIdParser
  where
    addrParser = addressOption @'AddressKindContract Nothing
      (#name :! "fa2-address")
      (#help :! "FA2 contract address")
    tokenIdParser = fmap TokenId . Opt.option (Opt.maybeReader readMaybe) $
      Opt.long "fa2-token-id" <>
      Opt.metavar "NATURAL" <>
      Opt.value 0 <>
      Opt.help "FA2 token type identifier" <>
      Opt.showDefault

contractMetadataConfigParser :: Opt.Parser ("metadataConfig" :! MetadataConfig)
contractMetadataConfigParser = fmap (#metadataConfig :!) $ MetadataConfig
  <$> nameParser <*> descParser
  where
    nameParser = Opt.strOption $
      Opt.long "contract-name" <>
      Opt.metavar "TEXT" <>
      Opt.value (mcName def) <>
      Opt.help "Contract name for TZIP-16 metadata" <>
      Opt.showDefault
    descParser = Opt.strOption $
      Opt.long "contract-description" <>
      Opt.metavar "TEXT" <>
      Opt.value (mcDescription def) <>
      Opt.help "Contract description for TZIP-16 metadata" <>
      Opt.showDefault

usageDoc :: Doc
usageDoc = mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  homebase-lite print --help", linebreak
   ]

storageParser :: Opt.Parser Storage
storageParser = initialStorage
  <$> adminParser
  <*> expireTimeParser
  <*> voteDelayParser
  <*> quorumThresholdParser
  <*> minimumBalanceParser
  <*> fa2ConfigParser
  <*> contractMetadataConfigParser
  where

    adminParser = (#admin :!) <$> (mkCLOptionParser @L1Address) Nothing
      (#name :! "admin")
      (#help :! "Initial admin address")
    toLong (_ :: Name s) = concat $ flip map (symbolVal $ Proxy @s) \case
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

instance HasCLReader L1Address where
  getMetavar = "CONTRACT OR IMPLICIT ADDRESS"
  getReader = do
    addrStr <- Opt.str
    case parseAddress addrStr of
      Right (MkAddress (addr :: KindedAddress kind')) -> case addr of
        ImplicitAddress{} -> pure $ Constrained addr
        ContractAddress{} -> pure $ Constrained addr
        TxRollupAddress{} ->
          Opt.readerError $ pretty $ nameF "Unexpected address kind" $
            "expected contract or implicit address, but got transaction rollup"
      Left err -> Opt.readerError $ pretty err

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
    compiledContract = toMichelsonContract lorentzContract

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
        buildMarkdownDoc $ attachDocCommons gitRev lorentzContract

    analyzerSubCmd =
      mkCommandParser "analyze" "Analyze the contract and prints statistics about it." $
        pure $ putTextLn $ pretty $ analyze $ unContractCode $ cCode compiledContract

    storageSubCmd = mkCommandParser "storage" "Print initial storage for the contract" $
      (<*> michelineOption) $
      storageParser <&> \storage useMicheline ->
        if useMicheline
        then BS.putStrLn $ encodePretty $ toExpression $ toVal storage
        else putStrLn $ printLorentzValue True storage

    originateSubCmd = mkCommandParser "originate" "Originate the contract. \
      \Note that the origination is run as the contract admin." $
      (<*> clientConfigParser) $
      (<*> storageParser) $
      nameOption <&> \name storage@Storage{..} cconf -> do
        putTextLn $ "Originating " <> name <> "..."
        env <- mkMorleyClientEnv cconf
        (_ , addr) <- runMorleyClientM @(OperationHash, ContractAddress) env $ do
          case sAdmin of
            Constrained a ->
              case isImplicitAddress a of
                Just Refl -> do
                  lOriginateContract OverwriteDuplicateAlias (ContractAlias name)
                    (AddressResolved a) zeroMutez
                    lorentzContract storage Nothing Nothing
                Nothing -> error "Admin needs to be an implicit address!"
        putStrLn $ "Originated " <> name <> " as " <> pretty addr

    versionSubCmd = mkCommandParser "version" "Show binary revision number" $
      pure $ putStrLn versionString

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
