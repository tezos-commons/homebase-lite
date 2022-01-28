-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE OverloadedLists #-}

module Main
  ( main
  ) where

import Data.Char (isUpper, toLower)
import GHC.TypeLits (symbolVal)
import Lorentz (DGitRevision(..), defaultContract)
import Lorentz.ContractRegistry
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import System.Environment (withProgName)

import Morley.CLI
import Morley.Tezos.Address
import Morley.Util.Named

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

programInfo :: DGitRevision -> Opt.ParserInfo (FA2Config, CmdLnArgs)
programInfo gitRev = Opt.info (
  (,) <$> fa2ConfigParser <*> argParser (contracts defFA2) gitRev <**> Opt.helper) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "This executable provides commands for development and interaction with the project."
  , Opt.header "Homebase-Lite"
  , Opt.footerDoc $ Just usageDoc
  ]

fa2ConfigParser :: Opt.Parser FA2Config
fa2ConfigParser = FA2Config <$> addrParser <*> tokenIdParser
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

defFA2 :: FA2Config
defFA2 = FA2Config
  { fa2Addr = [ta|KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9|] -- arbitrary address
  , fa2TokenId = TokenId 0
  }

contracts :: FA2Config -> ContractRegistry
contracts fa2conf = ContractRegistry $
  [ "Homebase-Lite" ?:: ContractInfo
    { ciContract = defaultContract $ homebaseLiteCode fa2conf
    , ciIsDocumented = True
    , ciStorageParser = Just storageParser
    , ciStorageNotes = Nothing
    }
  ]

storageParser :: Opt.Parser Storage
storageParser = initialStorage
  <$> adminParser
  <*> expireTimeParser
  <*> voteDelayParser
  <*> quorumThresholdParser
  <*> minimumBalanceParser
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

main :: IO ()
main = withUtf8 $ withProgName "homebase-lite" $ do
  (fa2conf, cmdLnArgs) <- Opt.execParser (programInfo DGitRevisionUnknown)
  runContractRegistry (contracts fa2conf) cmdLnArgs `catchAny` (die . displayException)
