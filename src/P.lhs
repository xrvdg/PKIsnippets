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

\begin{code}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
\end{code}

We need polykinds for the sublist and lastvect since we use them on both [*] as [*->*]
\begin{code}
{-# language PolyKinds #-}
\end{code}

Required for the splitvect handling
\begin{code}
{-# language UndecidableInstances #-}
\end{code}

Not strictly necessary but does make it easier to write code that uses unsafeCoerce in a somewhat safer manner
\begin{code}
{-# language ScopedTypeVariables #-}
\end{code}
\begin{code}
module P () where
import Control.Monad.Freer
import Control.Monad (void)
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Serializable as DS
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, newLocalNode)
import Unsafe.Coerce
\end{code}

TODO: What was the reason to have our own HVect.
  -> The hope was by changing the kind of HVect that it would be possible to get
     type inference. This however was not due to the limitation of HVect.
Defining our own HVect in this file doesn't work since we use polykinds
data HVect (xs :: [*]) where
  HNil :: HVect '[]
  (:&:) :: k -> HVect ts -> HVect (k ': ts)

\begin{code}
import Data.HVect as HV
\end{code}
Below is required for the test
\begin{code}
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import qualified Control.Monad.Freer.State as FS
import Control.Monad.IO.Class
import System.Exit hiding (ExitCode(ExitSuccess))
\end{code}

Singletons would probably work if the library was setup a little bit differently.
But we are working with GADTs so just dropping them in does not seem to work.
We would like to use the s* functions to work with lists.
But our handler is not promotable due to the GADTs it in turn depends on.
And just using them for the few type families it has has a too big cost.
import qualified Data.Singletons.Prelude.List as SL
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Singletons.Prelude.Num as SN
import qualified Data.Singletons.TH as STH

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

The following type families are not required since there are more specialized type families that have overlap with them.
The reason they are still included is because they have been useful during the exploratory phase when coming
up with specialized type families.
\begin{code}
type family AppendVect xs ys where
  AppendVect '[] bs = bs
  AppendVect (a ': as) bs = a ': (AppendVect as bs)

type family InitVect xs :: [k] where
   InitVect '[x] = '[]
   InitVect (t ': ts) = t ': (InitVect ts)

type family LTE n m where
  LTE Zero Zero = 'True
  LTE Zero (Succ Zero) = 'True
  LTE n Zero = 'False
  LTE (Succ n) (Succ m) = LTE n m
\end{code}

At first glance LastVect seem to perform the same operation as LastMember from freer-simple.
This is not the case, Last Member is only a type level computation using empty class/instances.
We sometimes need the last element of a list for type checking even though we do not care which
element this is (runListL). LastMember is about forcing the last member of a list.
\begin{code}
type family LastVect xs :: [k] where
   LastVect '[x] = '[x]
   LastVect (t ': ts) = LastVect ts
\end{code}

This is a verbatim copy of the SNatRep inside HVect, but the library author sadly did not export it.
\begin{code}
class SNatRep n where
    getSNat :: SNat n

instance SNatRep 'Zero where
    getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
    getSNat = SSucc getSNat
\end{code}

runList testcode
\begin{code}
test1 :: Eff '[FS.State Int] Int
test1 =  FS.get

test2 :: Eff '[FR.Reader Int] Int
test2 = FR.ask

test :: Eff '[FR.Reader Int, FS.State Int] Int
test = do n <- FS.get @Int
          m <- FR.ask
          FS.put (n + m)
          FS.put (n + m + 1)
          FS.put (n + m + 2)
          FS.get @Int

testRun :: Int
testRun = run (FS.evalState 3 (FR.runReader 5 test))

test1Run3 :: Int
test1Run3 = run (runList ((Handler (FS.evalState 3)) :&: HNil) test1)

test2Run3 :: Int
test2Run3 = run (runList (Handler (FR.runReader 5) :&: HNil) test2)

testRun3 :: Int
testRun3 = run (runList (Handler (FR.runReader 5) :&: Handler (FS.evalState 3) :&: HNil) test)
\end{code}

The console code is a slightly more eleborate test. The code is taken from the freer-simple documentation.
\begin{code}
data Console r where
  PutStrLn    :: String -> Console ()
  GetLine     :: Console String
  ExitSuccess :: Console ()

putStrLn' :: Member Console effs => String -> Eff effs ()
putStrLn' = send . PutStrLn

getLine' :: Member Console effs => Eff effs String
getLine' = send GetLine

exitSuccess' :: Member Console effs => Eff effs ()
exitSuccess' = send ExitSuccess

runConsole :: Eff '[Console, IO] a -> IO a
runConsole = runM . runConsole'

runConsole' :: (MonadIO m, LastMember m effs) => Eff (Console ': effs) a -> Eff effs a
runConsole' = interpretM (\case
  PutStrLn msg -> liftIO $ putStrLn msg
  GetLine -> liftIO $ getLine
  ExitSuccess -> liftIO $ exitSuccess)
\end{code}

\begin{code}
testIO :: Eff '[Console, IO] ()
testIO = do putStrLn' "Hello, World"
            exitSuccess'

testIORun :: IO ()
testIORun = runConsole testIO
\end{code}

The following is a nice test to check if adding constrains also works
\begin{code}
testIORun2 :: IO ()
testIORun2 = runHandlersM (Handler runConsole' :&: HNil) testIO
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

class SubListRep xs ys where
  getSubList :: SubList xs ys

instance SubListRep '[] '[] where
  getSubList = Base

instance {-# OVERLAPPING #-} SubListRep xs ys =>  SubListRep  (x ': xs) (x ': ys) where
  getSubList = Keep getSubList

instance {-# OVERLAPPABLE #-} SubListRep xs ys =>  SubListRep xs (y ': ys) where
  getSubList = Drop getSubList
\end{code}

While overlappinginstances is a little bit frowned up on. In this case
writing an overlapping instance is easier than coming up with a non-overlapping encoding.
See Dependent types using singletons for a non-overlapping version. However, that variant
might lean too heavy on singleton specific properties.
See the earlier remark about singletons, and why those do not work as-is with this
library.

\begin{code}
-- | Extract the sublist given the larger list with explicit evidence passing.
extractHVect' :: SubList xs ys -> HVect ys -> HVect xs
extractHVect' Base r = HNil
extractHVect' (Keep sl) (r :&: rs) = r :&: (extractHVect' sl rs)
extractHVect' (Drop sl) (r :&: rs) = extractHVect' sl rs

-- | Extract the sublist given the larger list with implicit evidence passing.
extractHVect :: (SubListRep xs ys) => HVect ys -> HVect xs
extractHVect = extractHVect' (getSubList)
\end{code}

This is a specialized version of extractHVect' since GHC type inference is not good enough.
To overcome this limitation we need to wrap clauses with unsafeCoerce.
\begin{code}
-- | Specialized version of 'extractHVect' for 'Handerlist'
extractHandler :: SubList (HandlerList s) (HandlerList r) -> HVect (HandlerList r) -> HVect (HandlerList s)
extractHandler Base r = HNil
extractHandler (Keep sl) (r :&: rs) = r :&: unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))
extractHandler (Drop sl) (r :&: rs) = unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))
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

\begin{code}
type family LenVect xs where
  LenVect '[] = Zero
  LenVect (t ': ts) = Succ (LenVect ts)
\end{code}

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
procCases _ (LiftIO io) = liftIO io
procCases _ (Send pid id) = DP.send pid id
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

\begin{code}
testProcIO :: IO ()
testProcIO = do
    backend <- initializeBackend "127.0.0.1" "8230" Node.initRemoteTable
    node <- newLocalNode backend
    Node.runProcess node (void testProc)

testProcIO2 :: IO ()
testProcIO2 = do
    backend <- initializeBackend "127.0.0.1" "8231" Node.initRemoteTable
    node <- newLocalNode backend
    Node.runProcess node (void testProc)

testProc :: DP.Process ()
testProc = runM (runProc handlers prog)
  where
    handlers = (Handler (FR.runReader "Hello") :&: Handler (FR.runReader (5 :: Int)) :&: HNil)
    prog = call f

testProc2 :: DP.Process PID
testProc2 = runM (runProc handlers prog)
  where
    handlers = (Handler (FR.runReader "Hello") :&: Handler (FR.runReader (5 :: Int)) :&: HNil)
    prog = spawn j
\end{code}

\begin{code}
hh :: Eff '[FR.Reader Int] Int
hh = FR.ask

gh :: Eff '[FR.Reader String] String
gh = FR.ask

\end{code}

Currently functions that what to use IO functionality need to be explicitely augmented with Proc '[], and have special functions
for those handlers to do the lifting.
This could be fixed by introducing an spawnIO/callIO functions or by going through the Union (The effs) which takes the content of the last entry
and puts a DP.liftIO before it. Maybe even generalizing it, by having a spawn function which you pass how it should reintepret it. A reintepret last.
This would require writing a replaceRelay function specialized to LastMember.

lesson: Break through layers of abstraction. Usually you do not want to have to know how something is implemented.
But when using unsafeCoerce, or other tricks to reduce code or even worse type level hacking, just peel back the curtains.
In this library we do make use of some properties of OpenUnion and effect code specific. The implementation is
perfect for what we want just the provided interface is not exactly what we need.

\begin{code}
f :: Eff [FR.Reader String, FR.Reader Int, Proc '[]] ()
f = do s <- FR.ask @String
       i <- FR.ask @Int
       sendIO $ putStrLn (s ++ show i)
\end{code}

lesson: implicitparams still requires passing the argument sometime and also gives you more
constraints to your functions. In this case it didn't add anything extra.

lesson: TODO substractive vs additive synthesis.

\begin{code}
sendIO :: (LastMember (Proc r) effs) => IO a -> Eff effs a
sendIO = sendL . LiftIO

g :: Eff '[FR.Reader Int, Proc '[FR.Reader String]] ()
g = do s <- call gh
       i <- FR.ask @Int
       sendIO $ putStrLn (s ++ show i)

h :: Eff '[FR.Reader String, Proc '[FR.Reader Int]] ()
h = do i <- call hh
       s <- FR.ask @String
       sendIO $ putStrLn (s ++ show i)

\end{code}

its type should still be a valid instance it just isn't that useful.
The code, however, shouldn't work

i :: Eff [Proc '[FR.Reader String], Proc '[FR.Reader Int]] ()
i = do i <- call hh
       s <- call gh
       sendM $ liftIO $ putStrLn (s ++ show i)

\begin{code}
j :: Eff '[Proc '[FR.Reader String, FR.Reader Int]] ()
j = do i <- call hh
       s <- call gh
       sendIO $ putStrLn (s ++ show i)
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


We want to split handlerlist into a non-proc and proc part.
For this we need strict take and drops therefore we introduce the following
type families.
\begin{code}
-- | type family for splitting type level lists and return HVect
type family SplitAtVect n xs where
  SplitAtVect n xs = (HVect (TakeVect n xs), HVect (DropVect n xs))

-- | Type family to take elements from a source list. This type
--  only succeeds when the source list is at least of size n.
-- Therefore having the garuantee/invariant that the length (TakeVec n xs) == n.
-- Which is different from term-level take in the prelude.
-- Note that this invariant is not always inferred by the compiler.
type family TakeVect n xs where
  TakeVect Zero xs = '[]
  TakeVect (Succ m) (x ': xs) = x ': TakeVect m xs

-- | Type family that only succeeds when the source list is
-- at least of size n.
-- Which is different from term-level drop in the prelude.
type family DropVect n xs where
  DropVect Zero xs = xs
  DropVect (Succ m) (x ': xs) = DropVect m xs

-- | General type family for splitting type level lists.
type family SplitVect n xs where
  SplitVect n xs = '(TakeVect n xs, DropVect n xs)

-- | Take the first n elements HVect xs which has a size of
-- at least n.
takeVect :: SNat n -> HVect xs -> HVect (TakeVect n xs)
takeVect SZero xs = HNil
takeVect (SSucc m) (r :&: rs) = r :&: (takeVect m rs)

-- | Drop the first n elements HVect xs which has a size of
-- at least n.
dropVect :: SNat n -> HVect xs -> HVect (DropVect n xs)
dropVect SZero xs = xs
dropVect (SSucc m) (r :&: rs) = dropVect m rs

-- | Split the HVect at position n without names for the type level lists
-- of the return vectors. The source vector needs to be at least of size n.
splitVect' :: SNat n -> HVect xs -> SplitAtVect n xs
splitVect' n xs = (takeVect n xs, dropVect n xs)

-- | Split the HVect at position n with the names for the type level lists of
-- the return vector. The source vector needs to be at least of size n.
splitVect :: (DropVect n ss ~ ks, TakeVect n ss ~ ss') => SNat n -> HVect ss -> (HVect ss', HVect ks)
splitVect n xs = splitVect' n xs
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
