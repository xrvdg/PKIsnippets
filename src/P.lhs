To allow type level lists
\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
Need Rank2Types more often than not
\begin{code}
{-# language RankNTypes #-}
\end{code}
To allow the definition of effects
\begin{code}
{-# language GADTs #-}
\end{code}
Type families are used heavy for all kinds of dependently typed programming.
For the more difficult cases typefamilydependencies helps the compiler resolve it.
\begin{code}
{-# language TypeFamilies, TypeFamilyDependencies #-}
\end{code}
Required for gets
\begin{code}
{-# language TypeApplications #-}
\end{code}
Makes writing handler a litte bit nicer
\begin{code}
{-# language LambdaCase #-}
\end{code}
required to be able to work with Member, Lastmember
\begin{code}
{-# language FlexibleContexts #-}
\end{code}

Not strictly necessary but does make it easier to write code that uses unsafeCoerce in a somewhat safer manner
\begin{code}
{-# language ScopedTypeVariables #-}
\end{code}
\begin{code}
module P (runProc, runList, runListL, runHandler, runHandlers, runHandlersM, spawn, call, sendIO) where
import Control.Monad.Freer
import Control.Monad (void)
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Serializable as DS
import Unsafe.Coerce
\end{code}

\begin{code}
import Data.HVect
import Data.HVect.Utils
import Data.SubList
\end{code}

Singletons would probably work if the library was setup a little bit differently.
But we are working with GADTs so just dropping them in does not seem to work.
We would like to use the s* functions to work with lists.
But our handler is not promotable due to the GADTs it in turn depends on.
And just using them for the few type families it has has a too big cost.

The handler functions do typically not depend on a fixed structure of the effect list, but
might have requirements for the last, and maybe even multiple members of the list.
By encoding the entire effects list the functions we store can do their requirement inspection.
Also tried to store constraints but that was harder. Making this datatype into a GADT might solve the
problem, but not sure if this is going to have a benefit.

\begin{code}
-- | The 'handler' data type allows to parameterize over the return type
--  of effect handler functions. See the source for why the effects 'effs' need
--- to be encoded. This data type is useful for creating function lists.
data Handler b effs = Handler (forall a. Eff (b ': effs) a -> Eff effs a)
\end{code}

Do note that a handler function can only work on a single function at the time.
This was done such that the handler list can be easily be lock-stepped to the
effect list. I am nearly certain that a flattened handler list equal to the effects lists
will also as a strong enough invariant with the rest of the functions based the way
the open union inside freer-simple is encoded. It would require some extra type families,
functions and type classes.

Injectivity is required to be able to use SubList on HandlerList.
\begin{code}
-- | When writing type signatures for effect 'Handler' lists
-- this type family takes care stripping a single effect for the
-- next handler in the list.
type family HandlerList effs = result | result -> effs where
  HandlerList '[] = '[]
  HandlerList (eff ': effs) = (Handler eff effs) ': HandlerList effs
\end{code}

We need an extra list for handlers such that the last entry is recored without having an handler.
This requirement is not satisfied by combing InitVect and HandlerList.
\begin{code}
-- | Same as 'HandlerList' but strip till the last one.
type family HandlerListL effs = result | result -> effs where
  HandlerListL '[eff, m] = '[Handler eff '[m]]
  HandlerListL (eff ': effs) = (Handler eff effs) ': HandlerListL effs
\end{code}

\begin{code}
-- | Run a a single handler on a effect.
runHandler :: Handler b effs -> Eff (b ': effs) a -> Eff effs a
runHandler (Handler f) = f

-- | Run all the handler on the effect and return the result of the effect.
-- Note that this function is non-monadic. See runHandlerM for the
-- monadic variant of this function.
runHandlers :: HVect (HandlerList effs) -> Eff effs a -> a
runHandlers hl eff = run (runList hl eff)

-- | Similar to 'runHandlers' but do not strip the pure effect shell.
runList :: HVect (HandlerList effs) -> Eff effs a -> Eff '[] a
runList HNil fect = unsafeCoerce fect
runList (r :&: rs) fect = runList (unsafeCoerce rs) (runHandler r' fect')
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs'
\end{code}

The code runList and runListL are practically the same. This code duplication
is acceptable since the code required to abstract over these two instances
will be more. If in the future a bug is found in either one of the two this
generalization should be considered.

\begin{code}
-- | Given a list that contains effect handlers such that when those are
--   ran only the last effect remains.
runListL :: HVect (HandlerListL effs) -> Eff effs a -> Eff (LastVect effs) a
runListL HNil fect = unsafeCoerce fect
runListL (r :&: rs) fect = unsafeCoerce (runListL (unsafeCoerce rs) (runHandler r' fect'))
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs'

-- | Monadic variant of 'runHandlers'
runHandlersM :: (LastVect effs ~ '[m], Monad m) => HVect (HandlerListL effs) -> Eff effs a -> m a
runHandlersM hl eff = runM (runListL hl eff)
\end{code}

TODO: lesson: don't try to combine -> substract



This is a verbatim copy of the SNatRep inside HVect, but the library author sadly did not export it.
\begin{code}
class SNatRep n where
    getSNat :: SNat n

instance SNatRep 'Zero where
    getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
    getSNat = SSucc getSNat
\end{code}

While overlappinginstances is a little bit frowned up on. In this case
writing an overlapping instance is easier than coming up with a non-overlapping encoding.
See Dependent types using singletons for a non-overlapping version. However, that variant
might lean too heavy on singleton specific properties.
See the earlier remark about singletons, and why those do not work as-is with this
library.

This is a specialized version of 'extractHVect' since GHC type inference is not good enough.
To overcome this limitation we need to wrap clauses with unsafeCoerce.
\begin{code}
-- | Specialized version of 'extractHVect' for 'Handerlist'
extractHandler :: SubList (HandlerList s) (HandlerList r) -> HVect (HandlerList r) -> HVect (HandlerList s)
extractHandler Base _ = HNil
extractHandler (Keep sl) (r :&: rs) = r :&: unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))
extractHandler (Drop sl) (_ :&: rs) = unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))
\end{code}

\begin{code}
-- | 'extractHandlers' extracts the sublist and splits this list at position 'n'.
extractHandlers :: forall n s r m ss dss tss rs.
  (ss ~ HandlerList s,
   rs ~ HandlerList r,
   tss ~ TakeVect n ss,
   dss ~ DropVect n ss,
   LenVect ss ~ m) =>
  SNat n -> SubList ss rs -> HVect rs -> (HVect tss, HVect dss)
extractHandlers n sl rs = splitVect n ss
  where ss :: HVect ss
        ss = extractHandler sl rs
\end{code}

lesson: use ScopedTypeVariables (and forall) to guide the compiler what the types should be.
This is often easier and requires less code than splitting it out into a seperate function.


Lesson: keep your steps as simple as possible. That way you might be able to use types without coercing
Lesson: You don't have to coerce if a type synonym suffices.
Lesson: Also allowed me to drop the polykinds extention
Lesson: there is such a thing as to much constraints. Both because they are harder to solve, and might lead
to weird infinite types.

==========================================
Process specific code
==========================================

In the future we might want to support more than just distributed-process, therefore we introduce a type synonym.
\begin{code}
type PID = DP.ProcessId
\end{code}

\begin{code}
data Proc (r :: [* -> *]) a where
  Spawn ::  (fs ~ FlattenProc s, ss ~ TillProc s, n ~ LenVect ss) => SNat n -> SubList fs r -> Eff s () -> Proc r PID
  Call ::  (fs ~ FlattenProc s, ss ~ TillProc s, n ~ LenVect ss) => SNat n -> SubList fs r -> Eff s a -> Proc r a
  Send  :: DS.Serializable a => PID -> a ->  Proc r ()
  Expect :: DS.Serializable a => Proc r a
  LiftIO :: IO a -> Proc r a
  Say :: String -> Proc r ()
\end{code}

\begin{code}
-- | Handler which translates Proc commands into the Process monad operations.
runProc :: HVect (HandlerList r) -> Eff '[Proc r] a -> Eff '[DP.Process] a
runProc hl effs = translate (procCases hl) effs

procCases :: HVect (HandlerList r) -> Proc r a -> DP.Process a
procCases hl (Spawn n sl eff) = procHandler (DP.spawnLocal . void) n (convertSublist sl) hl eff
procCases hl (Call n sl eff) = procHandler DP.callLocal n (convertSublist sl) hl eff
procCases _ (LiftIO io) = DP.liftIO io
procCases _ (Send pid a) = DP.send pid a
procCases _ Expect = DP.expect
procCases _ (Say str) = DP.say str

\end{code}

The sublist encoding of s and r, and the sublist of a function list on s and r are the same.
Therefore we can convert one in the other. Repacking the encoding doesn't work because the kinds
of lists parameters to the SubList are different.
\begin{code}
convertSublist :: SubList s r -> SubList (HandlerList s) (HandlerList r)
convertSublist = unsafeCoerce
\end{code}
Lesson: unsafeCoerce is love, unsafeCoerce is life.

The freer-simple library has support for sending to arbitrary member or the last member when that member is a monad.
We need to be able to send to the last member, but don't require that the last member is a monad.
\begin{code}
sendL :: (LastMember l effs) => l a -> Eff effs a
sendL = send
\end{code}

Lesson: sometimes specialized is a bad thing since the compiler cannot inference. Other times specialization
is the only way to really enforce something. Practically of this advice -> 0.

These two type families allows use to be polymorphic in the kind of effects we accept to spawn and call.
\begin{code}
type family TillProc xs where
  TillProc '[Proc ks] = '[]
  TillProc '[] = '[]
  TillProc (x ': xs) = x ': (TillProc xs)

type family GetProcContent xs where
  GetProcContent '[Proc ks] = ks
  GetProcContent '[] = '[]
  GetProcContent (x ': xs) = GetProcContent xs
\end{code}

Lesson: *Rep classes are useful but do not include them when you explicitely give
the result as parameter. Because then you still need to be able to satisfy the *Rep
constraint eventhough you already have the proof.

\begin{code}
call :: (
  SNatRep n,
  ss ~ (TillProc s),
  n ~ LenVect ss,
  LastMember (Proc r) effs,
  fhs ~ FlattenProc s,
  SubListRep fhs r)
     => Eff s a -> Eff effs a
call eff = sendL (Call getSNat getSubList eff)

spawn :: (
  SNatRep n,
  ss ~ (TillProc s),
  n ~ LenVect ss,
  LastVect s ~ '[Proc ks],
  LastMember (Proc r) effs,
  fhs ~ FlattenProc s,
  SubListRep fhs r)
     => Eff s () -> Eff effs PID
spawn eff = sendL (Spawn getSNat getSubList eff)

\end{code}

The unsafeCoerce are used here for two reasons.
- One to be able to use the splitted the handlerlist like they are supposed to.
- To lift an effect that doesn't contain a Proc to still go through the handler.
This way we do not have to do a runtime check of some type level trickery.
\begin{code}
procHandler :: forall a b fs ss hr fhs n s ks r. (
   fs ~ FlattenProc s,
   ss ~ TillProc s,
   hr ~ HandlerList r,
   fhs ~ HandlerList fs,
   ks ~ GetProcContent s) =>
   (DP.Process a -> DP.Process b) -> SNat n -> SubList fhs hr -> HVect hr -> Eff s a -> DP.Process b
procHandler f n sl hl eff = f (runM $ runProc ks' (unsafeCoerce (runListL ss' eff)))
  where (ss, ks) = extractHandlers n sl hl
        ss' :: HVect (HandlerListL s)
        ss' =  unsafeCoerce ss
        ks' :: HVect (HandlerList ks)
        ks' =  unsafeCoerce ks
\end{code}

lesson: Break through layers of abstraction. Usually you do not want to have to know how something is implemented.
But when using unsafeCoerce, or other tricks to reduce code or even worse type level hacking, just peel back the curtains.
In this library we do make use of some properties of OpenUnion and effect code specific. The implementation is
perfect for what we want just the provided interface is not exactly what we need.

lesson: implicitparams still requires passing the argument sometime and also gives you more
constraints to your functions. In this case it didn't add anything extra.

lesson: TODO substractive vs additive synthesis.

Currently functions that what to use IO functionality need to be explicitely augmented with Proc '[], and have special functions
for those handlers to do the lifting.
This could be fixed by introducing an spawnIO/callIO functions or by going through the Union (The effs) which takes the content of the last entry
and puts a DP.liftIO before it. Maybe even generalizing it, by having a spawn function which you pass how it should reintepret it. A reintepret last.
This would require writing a replaceRelay function specialized to LastMember.
\begin{code}
sendIO :: (LastMember (Proc r) effs) => IO a -> Eff effs a
sendIO = sendL . LiftIO
\end{code}

Since by convention we only use the last Proc for handling, when we flatten
the handler list we also assume that it is only at the end. Unpacking the
last element rather than every occurence of Proc made it easier to work
with the rest of type instances and code that assumes that proc is the last
entry.
\begin{code}
type family FlattenProc xs where
  FlattenProc '[Proc r] = r
  FlattenProc '[] = '[]
  FlattenProc (x ': xs) = x ': FlattenProc xs
\end{code}

lesson: reusability is low, very low. We have need to define things that have be done a thousand times
before just because we want to use it with kind polymorphism,
or because we can't lift them into singletons.

lesson: How I learned to stop worrying and love unsafeCoerce.
With the current state of Haskell type level machinery I think the best way might be
to write type families, try to get it as tight as possible, but when you are working
with functions which are 'trivially' true, just unsafeCoerce it.
An other possibility is to use empty type classes. Haven't explored that fully. Probably some proofs
could have be easier and more safely been derived using them.

lesson: Type level trickery is great tool for procrastinators. Being busy with
something related to the thing you are supposed to do, but not making any progress.

lesson: Dependent types: are you depending on types or are you a dependent type.

lesson: holes are beautiful

lesson: when working with constraints just write to fucntion and copy the type signature, and refine it bit by bit.
