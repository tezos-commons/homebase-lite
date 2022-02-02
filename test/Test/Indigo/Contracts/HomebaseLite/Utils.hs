-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Indigo.Contracts.HomebaseLite.Utils
  ( deployContract
  ) where

import Lorentz (Address, defaultContract)

import Fmt (Buildable(..))

import Morley.Util.Named (pattern (:!))
import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types

instance Buildable () where
  build () = "()"

deriving newtype instance Num Seconds

deployContract :: MonadCleveland caps m => m (Address, ContractHandle Parameter Storage ())
deployContract = do
  admin <- newAddress "admin"
  let stor = initialStorage
        (#admin :! admin)
        (#expireTime :! 3600)
        (#voteDelay :! 0)
        (#quorumThreshold :! 1)
        (#minimumBalance :! 1)
  contract <- originateSimple "HomebaseLite" stor $ defaultContract homebaseLiteCode
  pure (admin, contract)
