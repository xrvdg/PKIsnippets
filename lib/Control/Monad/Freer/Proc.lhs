\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
To allow the definition of effects
\begin{code}
{-# language GADTs #-}
{-# language RankNTypes #-}
\end{code}
Type families are used heavy for all kinds of dependently typed programming.
For the more difficult cases typefamilydependencies helps the compiler resolve it.
\begin{code}
{-# language TypeFamilies #-}
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
\begin{code}
{-# language ScopedTypeVariables #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language PolyKinds #-}
{-# language FunctionalDependencies #-}
\end{code}
\begin{code}
module Control.Monad.Freer.Proc (ProcConstraint, runIO, runProcess, TillProc, LenVect, SNatRep, SubListRep, FlattenProc, Proc, expect, runProc, say, spawn, call, send, liftIO) where
import Control.Monad.Freer hiding (send)
import qualified Control.Monad.Freer as F
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Node as Node
import qualified Control.Distributed.Process.Serializable as DS
\end{code}

\begin{code}
import Data.HVect
import Data.HVect.Utils
import Data.SubList
import Control.Monad (void)
import Control.Monad.Freer.Handler
import Unsafe.Coerce
\end{code}

TODO: lesson: don't try to combine -> substract



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
  RunIO ::  (a -> Eff '[Proc r] b) -> Proc r (a -> IO ())
  Send  :: DS.Serializable a => PID -> a ->  Proc r ()
  Expect :: DS.Serializable a => Proc r a
  LiftIO :: IO a -> Proc r a
  Say :: String -> Proc r ()
\end{code}

\begin{code}
runProcess :: Node.LocalNode -> HVect (HandlerList r) -> Eff '[Proc r] a -> IO ()
runProcess node hl effs = Node.runProcess node $ void (runM (runProc node hl effs))

-- | Handler which translates Proc commands into the Process monad operations.
runProc :: Node.LocalNode -> HVect (HandlerList r) -> Eff '[Proc r] a -> Eff '[DP.Process] a
runProc node hl effs = translate (procCases node hl) effs

procCases :: Node.LocalNode -> HVect (HandlerList r) -> Proc r a -> DP.Process a
procCases node hl (Spawn n sl eff) = procHandler node (DP.spawnLocal . void) n (convertSublist sl) hl eff
procCases node hl (Call n sl eff) = procHandler node DP.callLocal n (convertSublist sl) hl eff
procCases _ _ (LiftIO io) = DP.liftIO io
procCases node  hl (RunIO eff) = return (\a -> runProcess node hl (eff a))
procCases _ _ (Send pid a) = DP.send pid a
procCases _ _ Expect = DP.expect
procCases _ _  (Say str) = DP.say str

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
sendL = F.send
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

This is a verbatim copy of the SNatRep inside HVect, but the library author sadly did not export it.
\begin{code}
class SNatRep n where
    getSNat :: SNat n

instance SNatRep 'Zero where
    getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
    getSNat = SSucc getSNat
\end{code}

\begin{code}
class (SNatRep n, n ~ LenVect (TillProc s), SubListRep (FlattenProc s) (FlattenProc s), SubListRep (FlattenProc s) r, SubListRep r r) => ProcConstraint n s r where

instance (SNatRep n, n ~ LenVect (TillProc s), SubListRep (FlattenProc s) (FlattenProc s), SubListRep (FlattenProc s) r, SubListRep r r) => ProcConstraint n s r where

call :: (ProcConstraint n s r, LastMember (Proc r) effs) => Eff s a -> Eff effs a
call eff = sendL (Call getSNat getSubList eff)
\end{code}

  SNatRep n,
  ss ~ (TillProc s),
  n ~ LenVect ss,
  LastMember (Proc r) effs,
  fhs ~ FlattenProc s,
  SubListRep fhs r)

\begin{code}
spawn :: (ProcConstraint n s r, LastMember (Proc r) effs)
     => Eff s () -> Eff effs PID
spawn eff = sendL (Spawn getSNat getSubList eff)

runIO :: (LastMember (Proc r) effs) => (a -> Eff '[Proc r] b) -> Eff effs (a -> IO ())
runIO = sendL . RunIO

expect :: (DS.Serializable a, LastMember (Proc ks) r) => Eff r a
expect = sendL Expect

say :: (LastMember (Proc ks) r) => String -> Eff r ()
say = sendL . Say

send :: (DS.Serializable a, LastMember (Proc ks) r) => PID -> a -> Eff r ()
send pid a = sendL (Send pid a)
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
   Node.LocalNode -> (DP.Process a -> DP.Process b) -> SNat n -> SubList fhs hr -> HVect hr -> Eff s a -> DP.Process b
procHandler node f n sl hl eff = f (runM $ runProc node ks' (unsafeCoerce (runListL ss' eff)))
  where (ss, ks) = extractHandlers n sl hl
        ss' :: HVect (HandlerListL s)
        ss' =  unsafeCoerce ss
        ks' :: HVect (HandlerList ks)
        ks' =  unsafeCoerce ks
\end{code}

lesson: Break through layers of abstraction. Usually you do not want to have to know how something is implemented.
But when using unsafeCoerce, or other tricks to reduce code or even worse type level hacking, just peel back the curtains.
In this library we do make use of som
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
liftIO :: (LastMember (Proc r) effs) => IO a -> Eff effs a
liftIO = sendL . LiftIO
\end{code}

Since by convention we only use the last Proc for handling, when we flatten
the handler list we also assume that it is only at the end. Unpacking the
last element rather than every occurence of Proc made it easier to work
with the rest of type instances and code that assumes that proc is the last
entry.
\begin{code}
type family FlattenProc (xs :: [* -> *]) where
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
