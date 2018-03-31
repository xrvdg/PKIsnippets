To allow type level lists
\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
\begin{code}
{-# language TypeFamilies #-}
\end{code}
The split vect type families do not reduce the head therefore
we need to able UndecidableInstances.
\begin{code}
{-# language UndecidableInstances #-}
\end{code}
\begin{code}
{-# language PolyKinds #-}
\end{code}
\begin{code}
module Data.HVect.Utils (LastVect, TakeVect, DropVect, LenVect, takeVect, dropVect, splitVect, extractHVect', extractHVect) where

import Data.HVect
import Data.SubList

-- | Extract the sublist given the larger list with explicit evidence passing.
extractHVect' :: SubList xs ys -> HVect ys -> HVect xs
extractHVect' Base _ = HNil
extractHVect' (Keep sl) (r :&: rs) = r :&: (extractHVect' sl rs)
extractHVect' (Drop sl) (_ :&: rs) = extractHVect' sl rs

-- | Extract the sublist given the larger list with implicit evidence passing.
extractHVect :: (SubListRep xs ys) => HVect ys -> HVect xs
extractHVect = extractHVect' (getSubList)
\end{code}

We want to split handlerlist into a non-proc and proc part.
For this we need strict take and drops therefore we introduce the following
type families.
\begin{code}
type family LenVect xs where
  LenVect '[] = 'Zero
  LenVect (t ': ts) = 'Succ (LenVect ts)

-- | type family for splitting type level lists and return HVect
type family SplitAtVect n xs where
  SplitAtVect n xs = (HVect (TakeVect n xs), HVect (DropVect n xs))

-- | Type family to take elements from a source list. This type
--  only succeeds when the source list is at least of size n.
-- Therefore having the garuantee/invariant that the length (TakeVec n xs) == n.
-- Which is different from term-level take in the prelude.
-- Note that this invariant is not always inferred by the compiler.
type family TakeVect n xs where
  TakeVect 'Zero xs = '[]
  TakeVect ('Succ m) (x ': xs) = x ': TakeVect m xs

-- | Type family that only succeeds when the source list is
-- at least of size n.
-- Which is different from term-level drop in the prelude.
type family DropVect n xs where
  DropVect 'Zero xs = xs
  DropVect ('Succ m) (x ': xs) = DropVect m xs

-- | General type family for splitting type level lists.
type family SplitVect n xs where
  SplitVect n xs = '(TakeVect n xs, DropVect n xs)

\end{code}
Not using LTE proofs since that travels through the entire code base; even beyond 'procCases'.
I found that more harmful than using error cases for takeVect and dropVect.

type family LTE n m where
  LTE 'Zero 'Zero = 'True
  LTE 'Zero ('Succ 'Zero) = 'True
  LTE n 'Zero = 'False
  LTE ('Succ n) ('Succ m) = LTE n m

\begin{code}
-- | Take the first n elements HVect xs which has a size of
-- at least n.
takeVect :: SNat n -> HVect xs -> HVect (TakeVect n xs)
takeVect SZero _ = HNil
takeVect (SSucc _) HNil = error "takeVect: HVect xs has not enough elements"
takeVect (SSucc m) (r :&: rs) = r :&: (takeVect m rs)

-- | Drop the first n elements HVect xs which has a size of
-- at least n.
dropVect :: SNat n -> HVect xs -> HVect (DropVect n xs)
dropVect SZero xs = xs
dropVect (SSucc _) HNil = error "dropVect: HVect xs has not enough elements"
dropVect (SSucc m) (_ :&: rs) = dropVect m rs

-- | Split the HVect at position n with the names for the type level lists of
-- the return vector. The source vector needs to be at least of size n.
splitVect :: (DropVect n ss ~ ks, TakeVect n ss ~ ss') => SNat n -> HVect ss -> (HVect ss', HVect ks)
splitVect n xs = (takeVect n xs, dropVect n xs)
\end{code}

The following type families are not required since there are more specialized type families that have overlap with them.
The reason they are still included is because they have been useful during the exploratory phase when coming
up with specialized type families.
\begin{code}
type family AppendVect xs ys where
  AppendVect '[] bs = bs
  AppendVect (a ': as) bs = a ': (AppendVect as bs)

type family InitVect xs where
   InitVect '[x] = '[]
   InitVect (t ': ts) = t ': (InitVect ts)

\end{code}

At first glance LastVect seem to perform the same operation as LastMember from freer-simple.
This is not the case, Last Member is only a type level computation using empty class/instances.
We sometimes need the last element of a list for type checking even though we do not care which
element this is (runListL). LastMember is about forcing the last member of a list.
\begin{code}
type family LastVect xs where
   LastVect '[x] = '[x]
   LastVect (t ': ts) = LastVect ts
\end{code}
