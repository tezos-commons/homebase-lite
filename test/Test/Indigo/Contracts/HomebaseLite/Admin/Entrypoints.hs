-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Test.Indigo.Contracts.HomebaseLite.Admin.Entrypoints
  ( test_setAdmin
  , test_acceptAdmin
  , test_addMaintainers
  , test_removeMaintainers
  , test_configure
  ) where

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
      newAdmin <- toAddress <$> newFreshAddress "newAdmin"
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
  , testScenario "set_admin fails if sender is not admin" $ scenario do
      (_, contract) <- deployContract
      newAdmin <- newFreshAddress "newAdmin"
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        (transfer contract $ calling (ep @"Set_admin") (toAddress newAdmin))
        & expectCustomErrorNoArg #senderIsNotAdmin
  , testScenario "subsequent calls to set_admin replace candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- toAddress <$> newFreshAddress "newAdmin"
      newAdmin2 <- toAddress <$> newFreshAddress "newAdmin2"
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") newAdmin2
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin2
  , testScenario "calling set_admin with current admin invalidates candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- toAddress <$> newFreshAddress "newAdmin"
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") newAdmin
      (sAdminCandidateRPC <$> getStorage contract) @@== newAdmin
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") (toAddress admin)
      (sAdminCandidateRPC <$> getStorage contract) @@== (toAddress admin)
  ]

test_acceptAdmin :: [TestTree]
test_acceptAdmin =
  [ testScenario "accept_admin entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- newAddress "newAdmin"
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") (toAddress newAdmin)
      withSender newAdmin do
        transfer contract $ calling (ep @"Accept_admin") ()
      stor <- getStorage contract
      sAdminCandidateRPC stor @== (toAddress newAdmin)
      sAdminRPC stor @== (toAddress newAdmin)
  , testScenario "accept_admin fails when sender isn't admin candidate" $ scenario do
      (admin, contract) <- deployContract
      newAdmin <- toAddress <$> newFreshAddress "newAdmin"
      notNewAdmin <- newAddress "notNewAdmin"
      withSender admin do
        transfer contract $ calling (ep @"Set_admin") newAdmin
      withSender notNewAdmin do
        (transfer contract $ calling (ep @"Accept_admin") ())
        & expectCustomErrorNoArg #senderIsNotAdminCandidate
  ]

test_addMaintainers :: [TestTree]
test_addMaintainers =
  [ testScenario "add_maintainers entrypoint works" $ scenario do
      (admin, contract) <- deployContract
      newMaintainers <- traverse newFreshAddress $ replicate 3 auto
      withSender admin do
        transfer contract $ calling (ep @"Add_maintainers") (toAddress <$> newMaintainers)
      stor <- getStorage contract
      for_ (toAddress <$> newMaintainers) $
        (@@== ()) . (getBigMapValue (sMaintainersRPC stor))
  , testScenario "add_maintainers entrypoint works for duplicates" $ scenario do
      (admin, contract) <- deployContract
      newMaintainer <- toAddress <$> newFreshAddress "newMaintainer"
      withSender admin do
        transfer contract $ calling (ep @"Add_maintainers") [newMaintainer]
      bmId <- sMaintainersRPC <$> getStorage contract
      getBigMapValueMaybe bmId newMaintainer @@== Just ()
      getBigMapSize bmId @@== 1
  , testScenario "add_maintainers fails when sender is not admin" $ scenario do
      (_, contract) <- deployContract
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        (transfer contract $ calling (ep @"Add_maintainers") [toAddress notAdmin])
        & expectCustomErrorNoArg #senderIsNotAdmin
  ]

test_removeMaintainers :: [TestTree]
test_removeMaintainers =
  [ testScenario "remove_maintainers entrypoint works" $ scenario do
      (admin, contract, fmap toAddress -> newMaintainers) <- prepare
      withSender admin do
        transfer contract $ calling (ep @"Remove_maintainers") newMaintainers
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ newMaintainers $
        (@@== Nothing) . getBigMapValueMaybe bmId
  , testScenario "remove_maintainers fails when sender is not admin" $ scenario do
      (admin, contract) <- deployContract
      notAdmin <- newAddress "notAdmin"
      withSender notAdmin do
        transfer contract $ calling (ep @"Remove_maintainers") [toAddress admin]
        & expectCustomErrorNoArg #senderIsNotAdmin
  , testScenario "remove_maintainers succeeds with non-maintainer arguments" $ scenario do
      (admin, contract, newMaintainers) <- prepare
      notMaintainer <- newFreshAddress "notMaintainer"
      bmId <- sMaintainersRPC <$> getStorage contract
      oldSize <- getBigMapSize bmId
      withSender admin do
        transfer contract $ calling (ep @"Remove_maintainers") [toAddress notMaintainer]
      bmId' <- sMaintainersRPC <$> getStorage contract
      getBigMapSize bmId' @@== oldSize
      for_ (toAddress <$> newMaintainers) $
        (@@== ()) . (getBigMapValue bmId')
  , testScenario "remove_maintainers works for duplicates" $ scenario do
      (admin, contract, fmap toAddress -> newMaintainers) <- prepare
      let maint = head $ fromMaybe (error "impossible") $ nonEmpty newMaintainers
      withSender admin do
        transfer contract $ calling (ep @"Remove_maintainers") [maint, maint]
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ (drop 1 $ newMaintainers) $
        (@@== Just ()) . (getBigMapValueMaybe bmId)
      getBigMapValueMaybe bmId maint @@== Nothing
  ]
  where
    prepare
      :: MonadCleveland caps m
      => m ( ImplicitAddressWithAlias
           , ContractHandle Parameter Storage ()
           , [ImplicitAddressWithAlias]
           )
    prepare = do
      (admin, contract) <- deployContract
      newMaintainers <- traverse newFreshAddress $ replicate 3 auto
      withSender admin do
        transfer contract $ calling (ep @"Add_maintainers") (toAddress <$> newMaintainers)
      bmId <- sMaintainersRPC <$> getStorage contract
      for_ (toAddress <$> newMaintainers) $
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
        <$> (transfer contract $ calling (ep @"Add_maintainers") [toAddress admin])
        <*> (transfer contract $ calling (ep @"Configure") conf)
      Showing . sConfigurationRPC <$> getStorage contract @@== Showing (toRPC conf)
  , testScenario "configure fails when sender is not maintainer" $ scenario do
      (_admin, contract) <- deployContract
      notMaintainer <- newAddress "notMaintainer"
      withSender notMaintainer do
        transfer contract $ calling (ep @"Configure") Configuration
          { cExpireTime = 0
          , cVoteDelay = 0
          , cQuorumThreshold = 0
          , cMinimumBalance = 0
          }
        & expectCustomErrorNoArg #senderIsNotMaintainer
  ]
