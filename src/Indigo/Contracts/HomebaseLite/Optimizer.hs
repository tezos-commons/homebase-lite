-- SPDX-FileCopyrightText: 2022 Tezos Commons
-- SPDX-License-Identifier: LicenseRef-MIT-TC

module Indigo.Contracts.HomebaseLite.Optimizer
    ( optimize
    ) where

import Indigo (optimizeLorentzWithConf, (:->))

import Data.Default (def)
import Data.Type.Equality ((:~:)(Refl))
import Unsafe.Coerce (unsafeCoerce)

import Morley.Michelson.Optimizer (OptimizerConf, Rule(..), ocRuleset, orSimpleRule)
import Morley.Michelson.Typed (Dict(Dict), Instr(..), T(TBool), Value, pattern (:#))
import Morley.Util.Peano
  (Decrement, Drop, IsLongerOrSameLength, IsLongerThan, Take, pattern S, pattern Z)
import Morley.Util.PeanoNatural (PeanoNatural(..), fromPeanoNatural)
import Morley.Util.Type (KnownList, type (++))

optimize :: inp :-> out -> inp :-> out
optimize = optimizeLorentzWithConf optimizerConf

-- | Turn rule fixpoint into rule.
fixpoint :: (Rule -> Rule) -> Rule
fixpoint r = go
  where
    go :: Rule
    go = whileApplies (r go)

-- | Apply a rule to the same code, until it fails.
whileApplies :: Rule -> Rule
whileApplies r = Rule go
  where
    go :: Instr inp out -> Maybe (Instr inp out)
    go i = maybe (Just i) go (unRule r $ i)

flattenSeqLHS :: Rule -> Rule
flattenSeqLHS toplevel = Rule \case
  it@(Seq (Seq _ _) _) -> Just $ linearizeAndReapply toplevel it
  _                    -> Nothing

linearizeAndReapply :: Rule -> Instr inp out -> Instr inp out
linearizeAndReapply restart = \case
  Seq (Seq a b) c ->
    applyOnce restart $ Seq a (linearizeAndReapply restart (Seq b c))

  other -> applyOnce restart other

applyOnce :: Rule -> Instr inp out -> Instr inp out
applyOnce r i = maybe i id (unRule r $ i)

optimizerConf :: OptimizerConf
optimizerConf = def {
  ocRuleset = ocRuleset def <> fmap fixpoint
    (seqAdapter (flattenSeqLHS
      `orSimpleRule` unrollDrops
      `orSimpleRule` rollDup
      `orSimpleRule` rollDip)
      `orSimpleRule` dropNop
    : replicate 10 mainStage
    <>
     [seqAdapter (flattenSeqLHS
      `orSimpleRule` simpleSynonyms
      `orSimpleRule` rollDrops)
      `orSimpleRule` dropNop
     ]
    ) <> ocRuleset def
  }
  where
    mainStage =
      seqAdapter (flattenSeqLHS
        `orSimpleRule` dupDipDrop
        `orSimpleRule` pushDipDug
        `orSimpleRule` dupDugDrop
        `orSimpleRule` notIf
        `orSimpleRule` dupDipDownstream
        `orSimpleRule` digDug
        `orSimpleRule` redundantIf
        `orSimpleRule` nestedDip
        `orSimpleRule` emptyDip
        `orSimpleRule` dropFrame
        `orSimpleRule` dugDipSwap
        `orSimpleRule` mergeDips
        ) `orSimpleRule` dropNop

dropFrame :: Rule
dropFrame = Rule \case
  FrameInstr Proxy x :# xs -> Just . unsafeCoerce $ x :# unsafeCoerce xs
  _ -> Nothing

dropNop :: Rule
dropNop = Rule \case
  x :# Nop -> Just x
  Nop :# xs -> Just xs
  _ -> Nothing

rollDip :: Rule
rollDip = Rule \case
  DIP x :# xs -> Just $ DIPN One x :# xs
  _ -> Nothing

redundantIf :: Rule
redundantIf = Rule \case
  IF x y :# xs
    | x == y
    -> Just $ DROP :# x :# xs
  _ -> Nothing

emptyDip :: Rule
emptyDip = Rule \case
  DIPN _ Nop :# xs -> Just xs
  _ -> Nothing

nestedDip :: Rule
nestedDip = Rule \case
  (DIPN (n :: PeanoNatural n) (DIPN One (x :: Instr s s')) :: Instr inp out) :# xs
    | Refl :: IsLongerOrSameLength inp ('S n) :~: 'True <- unsafeCoerce Refl
    , Refl :: Take ('S n) inp ++ s :~: inp <- unsafeCoerce Refl
    , Refl :: Take ('S n) inp ++ s' :~: out <- unsafeCoerce Refl
    -> Just $ DIPN (Succ n) x :# xs
  (DIPN One (DIPN (n :: PeanoNatural n) (x :: Instr s s')) :: Instr inp out) :# xs
    | Refl :: IsLongerOrSameLength inp ('S n) :~: 'True <- unsafeCoerce Refl
    , Refl :: Take ('S n) inp ++ s :~: inp <- unsafeCoerce Refl
    , Refl :: Take ('S n) inp ++ s' :~: out <- unsafeCoerce Refl
    -> Just $ DIPN (Succ n) x :# xs
  _ -> Nothing

-- TODO [morley#663] [morley#299] remove this
unrollDrops :: Rule
unrollDrops = Rule \case
  (DROPN (Succ n) :: Instr inp out) :# xs
    -> case n of
      Zero -> Just . unsafeCoerce $ DROP :# xs
      (_ :: PeanoNatural n)
        | Refl :: IsLongerOrSameLength inp n :~: 'True <- unsafeCoerce Refl
        , Refl :: Drop n inp :~: a ': out <- unsafeCoerce Refl
        -> Just $ DROPN n :# DROP :# xs
  _ -> Nothing

rollDup :: Rule
rollDup = Rule \case
  DUP :# xs -> Just $ DUPN One :# xs
  _ -> Nothing

rollDrops :: Rule
rollDrops = Rule \case
  DROP :# DROP :# xs -> Just $ DROPN Two :# xs
  (DROPN (n :: PeanoNatural n) :: Instr inp t) :# (DROP :: Instr t out) :# xs
    | Refl :: IsLongerOrSameLength inp ('S n) :~: 'True <- unsafeCoerce Refl
    , Refl :: Drop ('S n) inp :~: out <- unsafeCoerce Refl
    -> Just $ DROPN (Succ n) :# xs
  (DROP :: Instr inp t) :# (DROPN (n :: PeanoNatural n) :: Instr t out) :# xs
    | Refl :: IsLongerOrSameLength inp ('S n) :~: 'True <- unsafeCoerce Refl
    , Refl :: Drop ('S n) inp :~: out <- unsafeCoerce Refl
    -> Just $ DROPN (Succ n) :# xs
  _ -> Nothing

seqAdapter :: (Rule -> Rule) -> (Rule -> Rule)
seqAdapter r topl = Rule \x -> unRule (r topl) x <|> (unRule (r topl) $ addNop x)
  where addNop :: Instr inp out -> Instr inp out
        addNop (Seq a b) = Seq a $ addNop b
        addNop x = x :# Nop

notIf :: Rule
notIf = Rule \case
  (NOT :: Instr inp b) :# IF a b :# xs
    | Refl :: inp :~: 'TBool ': ys <- unsafeCoerce Refl
    -> Just $ IF b a :# xs
  _ -> Nothing

dupDipDownstream :: Rule
dupDipDownstream = Rule \case
  (DUPN (n@(Succ _ :: PeanoNatural m) :: PeanoNatural n) :: Instr inp t)
    :# (DIPN (Succ (k :: PeanoNatural k)) (x :: Instr s s') :: Instr t out)
    :# xs
    | fromPeanoNatural n <= fromPeanoNatural k
    , Refl :: t' :~: Take k inp ++ s' <- unsafeCoerce Refl
    , Refl :: IsLongerOrSameLength t' n :~: 'True <- unsafeCoerce Refl
    , Refl :: Take (Decrement n) t' ++ (a ': Drop n t') :~: t' <- unsafeCoerce Refl
    , Refl :: out :~: a ': t' <- unsafeCoerce Refl
    -> Just $ (DIPN k x :: Instr inp t') :# DUPN n :# xs
  _ -> Nothing

dupDugDrop :: Rule
dupDugDrop = Rule \case
  (DUPN One :: Instr inp' inp)
    :# (DUG (Succ (m :: PeanoNatural m)) :: Instr inp out)
    :# (DROP :: Instr out out')
    :# xs
    | Refl :: inp :~: (a ': ys) <- unsafeCoerce Refl
    -> Just $ DUG @m @ys m :# xs
  _ -> Nothing

simpleSynonyms :: Rule
simpleSynonyms = Rule \case
  DUPN One :# xs -> Just $ DUP :# xs
  DIPN One x :# xs -> Just $ unsafeCoerce DIP x :# xs
  DROPN One :# xs -> Just $ unsafeCoerce DROP :# xs
  DUG One :# xs -> Just $ unsafeCoerce SWAP :# xs
  DIG One :# xs -> Just $ unsafeCoerce SWAP :# xs
  _ -> Nothing

pushDipDug :: Rule
pushDipDug = Rule \case
  (PUSH (a :: Value a) :: Instr inp t)
    :# DIPN (Succ x) (DROP :: Instr s1 s2)
    :# (DUG (y :: PeanoNatural n) :: Instr t1 out) :# xs
    | Refl :: (Take n inp) ++ (d ': s2) :~: inp <- unsafeCoerce Refl
    , Refl :: (Take n inp) ++ (a ': s2) :~: out <- unsafeCoerce Refl
    , Just Refl <- eqPeanoNat x y
    -> Just $ DIPN @n @inp @out @(d ': s2) @(a ': s2) y (DROP :# PUSH a) :# xs
  _ -> Nothing

eqPeanoNat :: PeanoNatural n -> PeanoNatural m -> Maybe (n :~: m)
eqPeanoNat x y
  | fromPeanoNatural x == fromPeanoNatural y
  = Just $ unsafeCoerce Refl
  | otherwise = Nothing

digDug :: Rule
digDug = Rule \case
  (DIG x :: Instr inp t) :# (DUG y :: Instr t out) :# xs
    | Just Refl <- eqPeanoNat x y
    -> Just $ unsafeCoerce xs
  _ -> Nothing

dupDipDrop :: Rule
dupDipDrop = Rule \case
  (DUPN (Succ (x :: PeanoNatural n)) :: Instr inp t)
    :# (DIPN (Succ (y :: PeanoNatural m)) DROP :: Instr t out) :# xs
    | Refl :: out :~: (a ': Take n inp ++ Drop ('S n) inp) <- unsafeCoerce Refl
    , Refl :: IsLongerThan inp n :~: 'True <- unsafeCoerce Refl
    , Just Refl <- eqPeanoNat x y
    , Dict :: Dict (KnownList inp) <- unsafeCoerce (Dict :: Dict (KnownList '[]))
    -- Pulling KnownList from the aether here is "safe" because the dict is only
    -- used in a compile-time proof.
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
    , Refl :: Take m (b : Drop ('S 'Z) inp) ++ s' :~: d <- unsafeCoerce Refl
    , Refl :: d :~: a : Drop ('S 'Z) d <- unsafeCoerce Refl
    , Refl :: Take n (Drop ('S 'Z) d) ++ (a : Drop ('S n) d) :~: out <- unsafeCoerce Refl
    , Refl :: inp :~: b ': Drop ('S 'Z) inp <- unsafeCoerce Refl
    , Refl :: Take m (b : Drop ('S 'Z) inp) ++ s :~: b : Drop ('S 'Z) inp <- unsafeCoerce Refl
    -> Just $ DIPN m i :# DUG n :# xs
  _ -> Nothing

mergeDips :: Rule
mergeDips = Rule \case
  (DIPN (n :: PeanoNatural n) (i :: Instr p p') :: Instr inp t)
    :# (DIPN (m :: PeanoNatural m) (j :: Instr s s') :: Instr t out)
    :# xs
    | Just Refl <- eqPeanoNat n m
    , Refl :: p' :~: s <- unsafeCoerce Refl
    , Refl :: Take n inp ++ s' :~: out <- unsafeCoerce Refl
    -> Just $ DIPN n (i :# j) :# xs
  _ -> Nothing
