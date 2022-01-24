-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Main
  ( main
  ) where

import Test.Cleveland.Ingredients (ourIngredients)
import Test.Cleveland.Tasty (clevelandMainWithIngredients)

import Tree (tests)

main :: IO ()
main = tests >>= clevelandMainWithIngredients ourIngredients
