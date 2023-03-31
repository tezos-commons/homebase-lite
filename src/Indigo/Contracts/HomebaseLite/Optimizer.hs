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
import Morley.Michelson.Typed (Instr(..), Value)
import Morley.Util.Peano
  (Decrement, Drop, IsLongerOrSameLength, IsLongerThan, LazyTake, Tail, Take, pattern S, pattern Z)
import Morley.Util.PeanoNatural (PeanoNatural(..), eqPeanoNat, fromPeanoNatural)
import Morley.Util.Type (type (++))

pattern (:#) :: Instr inp b -> Instr b out -> Instr inp out
pattern l :# r <- (maybeSeq -> Seq l r)
  where l :# Nop = l
        Nop :# r = r
        l :# r = Seq l r
infixr 8 :#

maybeSeq :: Instr inp out -> Instr inp out
maybeSeq = \case
  x@Seq{} -> x
  x -> Seq x Nop

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
    & alterRulesAtPrio (dupDugDrop :) (OptimizationStageMain 0)
    & alterRulesAtPrio (dipSwapDrop :) (OptimizationStageRollAdjacent (-1))
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
      (DUPN (n@(Succ _ :: PeanoNatural m) :: PeanoNatural n) :: Instr inp t)
        :# (DIPN (Succ (k :: PeanoNatural k)) (x :: Instr s s') :: Instr t out)
        :# xs
        | fromPeanoNatural n <= fromPeanoNatural k
        , Refl :: t' :~: Take k inp ++ s' <- unsafeCoerce Refl
        , Refl :: IsLongerOrSameLength t' n :~: 'True <- unsafeCoerce Refl
        , Refl :: LazyTake (Decrement n) t' ++ (a ': Drop n t') :~: t' <- unsafeCoerce Refl
        , Refl :: out :~: a ': t' <- unsafeCoerce Refl
        -> Just $ (DIPN k x :: Instr inp t') :# DUPN n :# xs
      _ -> Nothing

dupDugDrop :: Rule
dupDugDrop = Rule \case
  DUP :# (DUG (Succ m) :: Instr inp out) :# DROP :# xs
    -> Just $ DUG @_ @(Tail inp) m :# xs
  _ -> Nothing

pushDipDug :: Rule
pushDipDug = Rule \case
  (PUSH (a :: Value a) :: Instr inp t)
    :# DIPN (Succ x) (DROP :: Instr s1 s2)
    :# (DUG (y :: PeanoNatural n) :: Instr t1 out) :# xs
    | Refl :: (LazyTake n inp) ++ (d ': s2) :~: inp <- unsafeCoerce Refl
    , Refl :: (LazyTake n inp) ++ (a ': s2) :~: out <- unsafeCoerce Refl
    , Refl :: (d ': s2) :~: Drop n inp <- unsafeCoerce Refl
    , Refl :: (a ': s2) :~: Drop n out <- unsafeCoerce Refl
    , Just Refl <- eqPeanoNat x y
    -> Just $ DIPN @n @inp @out @(d ': s2) @(a ': s2) y (DROP :# PUSH a) :# xs
  _ -> Nothing

dupDipDrop :: Rule
dupDipDrop = Rule \case
  (DUPN (Succ (x :: PeanoNatural n)) :: Instr inp t)
    :# (DIPN (Succ (y :: PeanoNatural m)) DROP :: Instr t out) :# xs
    | Refl :: out :~: a ': LazyTake n inp ++ Drop ('S n) inp <- unsafeCoerce Refl
    , Refl :: inp :~: LazyTake n (Drop ('S 'Z) out) ++ a ': Drop ('S n) out <- unsafeCoerce Refl
    , Refl :: IsLongerThan inp n :~: 'True <- unsafeCoerce Refl
    , Just Refl <- eqPeanoNat x y
    -> Just $ DIG x :# xs
  _ -> Nothing

dugDipSwap :: Rule
dugDipSwap = Rule \case
  (DUG (n :: PeanoNatural n) :: Instr inp t)
    :# (DIPN (m :: PeanoNatural m) (i :: Instr s s') :: Instr t out)
    :# xs
    | fromPeanoNatural n < fromPeanoNatural m
    , Refl :: IsLongerOrSameLength inp m :~: 'True <- unsafeCoerce Refl
    , Refl :: IsLongerThan out n :~: 'True <- unsafeCoerce Refl
    , Refl :: LazyTake m (b : Drop ('S 'Z) inp) ++ s' :~: d <- unsafeCoerce Refl
    , Refl :: s' :~: Drop m d <- unsafeCoerce Refl
    , Refl :: LazyTake n (Drop ('S 'Z) d) ++ (a : Drop ('S n) d) :~: out <- unsafeCoerce Refl
    , Refl :: a : LazyTake n out ++ Drop ('S n) out :~: d <- unsafeCoerce Refl
    , Refl :: inp :~: b ': Drop ('S 'Z) inp <- unsafeCoerce Refl
    , Refl :: LazyTake m (b : Drop ('S 'Z) inp) ++ s :~: b : Drop ('S 'Z) inp <- unsafeCoerce Refl
    , Refl :: s :~: Drop m (b : Drop ('S 'Z) inp) <- unsafeCoerce Refl
    -> Just $ DIPN m i :# DUG n :# xs
  _ -> Nothing

dipSwapDrop :: Rule
dipSwapDrop = Rule \case
  DIP (SWAP :# DROP) -> Just $ DIPN Two DROP
  _ -> Nothing
