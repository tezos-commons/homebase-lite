-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# LANGUAGE OverloadedLists #-}

module Main
  ( main
  ) where

import Data.Char (isUpper, toLower)
import GHC.TypeLits (symbolVal)
import Lorentz (DGitRevision(..), GitRepoSettings(..), defaultContract, mkDGitRevision)
import Lorentz.ContractRegistry
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as Opt
import Options.Applicative.Help.Pretty (Doc, linebreak)
import System.Environment (withProgName)

import Morley.CLI
import Morley.Util.Named

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

programInfo :: DGitRevision -> Opt.ParserInfo CmdLnArgs
programInfo gitRev = Opt.info (Opt.helper <*> argParser contracts gitRev) $
  mconcat
  [ Opt.fullDesc
  , Opt.progDesc "This executable provides commands for development and interaction with the project."
  , Opt.header "Homebase-Lite"
  , Opt.footerDoc $ Just usageDoc
  ]

usageDoc :: Doc
usageDoc = mconcat
   [ "You can use help for specific COMMAND", linebreak
   , "EXAMPLE:", linebreak
   , "  homebase-lite print --help", linebreak
   ]

contracts :: ContractRegistry
contracts = ContractRegistry $
  [ "Homebase-Lite" ?:: ContractInfo
    { ciContract = defaultContract homebaseLiteCode
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
  cmdLnArgs <- Opt.execParser . programInfo $
    $mkDGitRevision $ GitRepoSettings $ mappend "https://github.com/tezos-commons/homebase-lite/commit/"
  runContractRegistry contracts cmdLnArgs `catchAny` (die . displayException)
