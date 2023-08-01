-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Optimizer
    ( optimize
    , optimizerConf
    ) where

import Indigo (optimizeLorentzWithConf, (:->))

import Data.Default (def)
import Data.Type.Equality ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)

import Morley.Michelson.Optimizer hiding (optimize)
import Morley.Michelson.Optimizer.Utils (pattern (:#))
import Morley.Michelson.Typed (DupableScope, Instr(..))
import Morley.Util.Peano (Drop, IsLongerOrSameLength, IsLongerThan, LazyTake, pattern S, pattern Z)
import Morley.Util.PeanoNatural (PeanoNatural(..), eqPeanoNat, fromPeanoNatural)
import Morley.Util.Type (type (++))

optimize :: inp :-> out -> inp :-> out
optimize = fixpoint $ optimizeLorentzWithConf optimizerConf
  where
    -- this is rather ugly, but 'dupDipDownstream' etc have to be run after the
    -- main optimization pass, as main pass doesn't use DIPN, and they can
    -- expose additional optimizations for the main stage.
    fixpoint f x
      | res == x = x
      | otherwise = fixpoint f res
      where res = f x

optimizerConf :: OptimizerConf
optimizerConf = def
  { ocRuleset = defaultRules
    & alterRulesAtPrio (aggRules <>) (OptimizationStageRollAdjacent 1)
  }
  where
    aggRules =
      [ dupDipDownstream
      , pushDipDug
      , dupDipDrop
      , dugDipSwap
      ]

dupDipDownstream :: Rule
dupDipDownstream = Rule go
  where
    go :: Instr inp out -> Maybe (Instr inp out)
    go = \case
      DUP :# DIP arg :# xs -> go $ DUPN One :# DIPN One arg :# xs
      DUPN n :# DIP arg :# xs -> go $ DUPN n :# DIPN One arg :# xs
      DUP :# DIPN m arg :# xs -> go $ DUPN One :# DIPN m arg :# xs
      DUPN n@Succ{} :# DIPN (Succ k) x :# xs
        | fromPeanoNatural n <= fromPeanoNatural k
        -> Just $ DIPN k x :# unsafeDupn n :# xs
      _ -> Nothing

    -- GHC has some trouble picking instances in-context, hence a separate function
    unsafeDupn :: forall n inp a. DupableScope a => PeanoNatural ('S n) -> Instr inp (a : inp)
    unsafeDupn n
      | Refl :: IsLongerOrSameLength inp ('S n) :~: 'True <- unsafeCoerce Refl
      , Refl :: LazyTake n inp ++ (a ': Drop ('S n) inp) :~: inp <- unsafeCoerce Refl
      = DUPN n

pushDipDug :: Rule
pushDipDug = Rule \case
  PUSH @a @inp a :# DIPN (Succ x) (DROP @d @s2) :# DUG @n @_ @out y :# xs
    | Just Refl <- eqPeanoNat x y
    , Refl :: (LazyTake n inp) ++ (a ': s2) :~: out <- unsafeCoerce Refl
    , Refl :: (a ': s2) :~: Drop n out <- unsafeCoerce Refl
    -> Just $ DIPN @n @inp @out @(d ': s2) @(a ': s2) y (DROP :# PUSH a) :# xs
  _ -> Nothing

dupDipDrop :: Rule
dupDipDrop = Rule \case
  DUPN @_ @inp @_ @a (Succ @_ @n x) :# DIPN @_ @_ @out (Succ @_ @m y) DROP :# xs
    | Refl :: out :~: a ': LazyTake n inp ++ Drop ('S n) inp <- unsafeCoerce Refl
    , Refl :: inp :~: LazyTake n (Drop ('S 'Z) out) ++ a ': Drop ('S n) out <- unsafeCoerce Refl
    , Refl :: IsLongerThan inp n :~: 'True <- unsafeCoerce Refl
    , Just Refl <- eqPeanoNat x y
    -> Just $ DIG x :# xs
  _ -> Nothing

dugDipSwap :: Rule
dugDipSwap = Rule \case
  DUG @n @inp @d n :# DIPN @m @_ @_ @s @s' m i :# xs
    | fromPeanoNatural n < fromPeanoNatural m
    , Refl :: IsLongerOrSameLength inp m :~: 'True <- unsafeCoerce Refl
    , Refl :: LazyTake m inp ++ s :~: inp <- unsafeCoerce Refl
    , Refl :: s' :~: Drop m d <- unsafeCoerce Refl
    , Refl :: s :~: Drop m inp <- unsafeCoerce Refl
    -> Just $ DIPN m i :# DUG n :# xs
  _ -> Nothing
