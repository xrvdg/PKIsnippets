To allow type level lists
\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
We need polykinds for the sublist and lastvect since we use them on both [*] as [*->*]
\begin{code}
{-# language PolyKinds #-}
\end{code}
The sublists have more than parameter.
\begin{code}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
\end{code}
\begin{code}
{-# language GADTs #-}
{-# language UndecidableInstances #-}
\end{code}
\begin{code}
module Data.SubList (SubList(..), SubListRep, SubListRefl, SubListTrans, getSubList) where
\end{code}
We want to be able that we can reduce the required effects. To be able to determine whether
the parent procedure has these handlers and to make this subset selection automatically from
the perspective of the library user we need SubList.
The dataype encodes the proof and SubListRep gives us a method the extract this encoding.
\begin{code}
-- | Datatype to encode in what way xs is a sublist of ys.
-- Note that this is about sublist and not prefix or subsequences.
data SubList (xs :: k) (ys :: k) where
  Base :: SubList '[] '[]
  Keep :: SubList xs ys -> SubList (x ': xs) (x ': ys)
  Drop :: SubList xs ys -> SubList xs (y ': ys)

class (SubListRep xs xs) => SubListRefl xs

instance (SubListRep xs xs) => SubListRefl xs

class (SubListRefl xs, SubListRefl ys, SubListRefl zs, SubListRep xs ys, SubListRep ys zs, SubListRep xs zs) => SubListTrans xs ys zs

instance (SubListRefl xs, SubListRefl ys, SubListRefl zs, SubListRep xs ys, SubListRep ys zs, SubListRep xs zs) => SubListTrans xs ys zs

class SubListRep xs ys where
  getSubList :: SubList xs ys

instance SubListRep '[] '[] where
  getSubList = Base

instance {-# OVERLAPPING #-} (SubListRep xs ys) =>  SubListRep  (x ': xs) (x ': ys) where
  getSubList = Keep getSubList

instance {-# OVERLAPPABLE #-} (SubListRep xs ys) =>  SubListRep xs (y ': ys) where
  getSubList = Drop getSubList

\end{code}

While overlappinginstances is a little bit frowned up on. In this case
writing an overlapping instance is easier than coming up with a non-overlapping encoding.
See Dependent types using singletons for a non-overlapping version. However, that variant
might lean too heavy on singleton specific properties.
See the earlier remark about singletons, and why those do not work as-is with this
library.
