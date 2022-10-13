-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.Admin.Entrypoints
  ( test_setAdmin
  , test_acceptAdmin
  , test_addMaintainers
  , test_removeMaintainers
  , test_configure
  ) where

import Lorentz (Address)

import Test.Tasty (TestTree)

import Test.Cleveland

import Indigo.Contracts.HomebaseLite
import Indigo.Contracts.HomebaseLite.Types
import Test.Indigo.Contracts.HomebaseLite.AsRPC
import Test.Indigo.Contracts.HomebaseLite.Utils

test_setAdmin :: [TestTree]
test_setAdmin =
  [ testScenario "set_admin entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      withSender admin do
        call contract (Call @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
  , testScenario "set_admin fails if sender is not admin" $ scenario do
      (_, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        call contract (Call @"Set_admin") newAdmin
        & expectCustomErrorNoArg #senderIsNotAdmin
  , testScenario "subsequent calls to set_admin replace candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      newAdmin2 <- newFreshAddress "newAdmin2"
      withSender admin do
        call contract (Call @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
      withSender admin do
        call contract (Call @"Set_admin") newAdmin2
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin2
  , testScenario "calling set_admin with current admin invalidates candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      withSender admin do
        call contract (Call @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
      withSender admin do
        call contract (Call @"Set_admin") admin
      (sAdminCandidateRPC <$> getStorage contract) @@== admin
  ]

test_acceptAdmin :: [TestTree]
test_acceptAdmin =
  [ testScenario "accept_admin entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newAddress "newAdmin"
      withSender admin do
        call contract (Call @"Set_admin") newAdmin
      withSender newAdmin do
        call contract (Call @"Accept_admin") ()
      stor <- getStorage contract
      sAdminCandidateRPC stor @== newAdmin
      sAdminRPC stor @== newAdmin
  , testScenario "accept_admin fails when sender isn't admin candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      notNewAdmin <- newAddress "notNewAdmin"
      withSender admin do
        call contract (Call @"Set_admin") newAdmin
      withSender notNewAdmin do
        call contract (Call @"Accept_admin") ()
        & expectCustomErrorNoArg #senderIsNotAdminCandidate
  ]

test_addMaintainers :: [TestTree]
test_addMaintainers =
  [ testScenario "add_maintainers entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      newMaintainers <- traverse newFreshAddress $ replicate 3 auto
      withSender admin do
        call contract (Call @"Add_maintainers") newMaintainers
      stor <- getStorage contract
      for_ newMaintainers $
        (@@== ()) . (getBigMapValue (sMaintainersRPC stor))
  , testScenario "add_maintainers entrypoint works for duplicates" $ scenario do
      (admin, contract) <- deployContract
      newMaintainer <- newFreshAddress "newMaintainer"
      withSender admin do
        call contract (Call @"Add_maintainers") [newMaintainer]
      bmId <- sMaintainersRPC <$> getStorage contract
      getBigMapValueMaybe bmId newMaintainer @@== Just ()
      getBigMapSize bmId @@== 1
  , testScenario "add_maintainers fails when sender is not admin" $ scenario do
      (_, contract) <- deployContract
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        call contract (Call @"Add_maintainers") [notAdmin]
        & expectCustomErrorNoArg #senderIsNotAdmin
  ]

test_removeMaintainers :: [TestTree]
test_removeMaintainers =
  [ testScenario "remove_maintainers entrypoint works" $ scenario do
      (admin, contract, newMaintainers) <- prepare
      withSender admin do
        call contract (Call @"Remove_maintainers") newMaintainers
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ newMaintainers $
        (@@== Nothing) . getBigMapValueMaybe bmId
  , testScenario "remove_maintainers fails when sender is not admin" $ scenario do
      (admin, contract) <- deployContract
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        call contract (Call @"Remove_maintainers") [admin]
        & expectCustomErrorNoArg #senderIsNotAdmin
  , testScenario "remove_maintainers succeeds with non-maintainer arguments" $ scenario do
      (admin, contract, newMaintainers) <- prepare
      notMaintainer <- newFreshAddress "notMaintainer"
      bmId <- sMaintainersRPC <$> getStorage contract
      oldSize <- getBigMapSize bmId
      withSender admin do
        call contract (Call @"Remove_maintainers") [notMaintainer]
      bmId' <- sMaintainersRPC <$> getStorage contract
      getBigMapSize bmId' @@== oldSize
      for_ newMaintainers $
        (@@== ()) . (getBigMapValue bmId')
  , testScenario "remove_maintainers works for duplicates" $ scenario do
      (admin, contract, newMaintainers) <- prepare
      let maint = head $ fromMaybe (error "impossible") $ nonEmpty newMaintainers
      withSender admin do
        call contract (Call @"Remove_maintainers") [maint, maint]
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ (drop 1 $ newMaintainers) $
        (@@== Just ()) . (getBigMapValueMaybe bmId)
      getBigMapValueMaybe bmId maint @@== Nothing
  ]
  where
    prepare :: MonadCleveland caps m => m (Address, ContractHandle Parameter Storage (),
                       [Address])
    prepare = do
      (admin, contract) <- deployContract
      newMaintainers <- traverse newFreshAddress $ replicate 3 auto
      withSender admin do
        call contract (Call @"Add_maintainers") newMaintainers
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ newMaintainers $
        (@@== ()) . (getBigMapValue bmId)
      pure (admin, contract, newMaintainers)

test_configure :: [TestTree]
test_configure =
  [ testScenario "configure entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      let conf = Configuration
            { cExpireTime = 10
            , cVoteDelay = 20
            , cQuorumThreshold = 30
            , cMinimumBalance = 40
            }
      void $ withSender admin $ inBatch $ (,)
        <$> call contract (Call @"Add_maintainers") [admin]
        <*> call contract (Call @"Configure") conf
      Showing . sConfigurationRPC <$> getStorage contract @@== Showing (toRPC conf)
  , testScenario "configure fails when sender is not maintainer" $ scenario do
      (_admin, contract) <- deployContract
      notMaintainer <- newAddress "notMaintainer"
      withSender notMaintainer do
        call contract (Call @"Configure") Configuration
          { cExpireTime = 0
          , cVoteDelay = 0
          , cQuorumThreshold = 0
          , cMinimumBalance = 0
          }
        & expectCustomErrorNoArg #senderIsNotMaintainer
  ]
