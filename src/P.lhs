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
import Data.HVect as HV
import qualified Control.Distributed.Process as DP
import qualified Control.Distributed.Process.Serializable as DS
import qualified Control.Distributed.Process.Node as Node
import Control.Distributed.Process.Backend.SimpleLocalnet (initializeBackend, newLocalNode)
import Unsafe.Coerce
\end{code}

Below is required for the test
\begin{code}
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import qualified Control.Monad.Freer.State as FS
import Control.Monad.IO.Class
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

For the IO tests
\begin{code}
import System.Exit hiding (ExitCode(ExitSuccess))
\end{code}

Handler has an explicit effs such that constraints that handlers have are easily checked.
Trying to also store constraints seemed to be more difficult.
\begin{code}

data Handler b effs = Handler (forall a. Eff (b ': effs) a -> Eff effs a)
\end{code}

Defining our own HVect in this file doesn't work since we use polykinds
data HVect (xs :: [*]) where
  HNil :: HVect '[]
  (:&:) :: k -> HVect ts -> HVect (k ': ts)

runList :: forall eff effs a . HVect (HandlerList (eff ': effs) a) -> Eff (eff ': effs) a -> Eff '[] a
runList (r :&: HNil) fect = let r' = (unsafeCoerce r) :: (Handler eff '[] a)
                                fect' = (unsafeCoerce fect) :: Eff '[eff] a
                            in r' fect'
runList (r :&: rs) fect =  let r' = (unsafeCoerce r) :: (Handler eff (eff' ': effs') a)
                               fect' = (unsafeCoerce rs) :: Eff (eff ': eff' ': effs') a
                               rs' = (unsafeCoerce rs) :: HVect (HandlerList (eff' ': effs') a)
                           in runList rs' (r' fect')

Misschien iets van een length check voor HandlerList en effs toevoegen? Op die manier zijn de twee wellicht beter
te reducren voor de compiler
\begin{code}

runHandler :: Handler b effs -> Eff (b ': effs) a -> Eff effs a
runHandler (Handler f) = f

runHandlers :: HVect (HandlerList effs) -> Eff effs a -> a
runHandlers hl eff = run (runList hl eff)

runList :: HVect (HandlerList effs) -> Eff effs a -> Eff '[] a
runList HNil fect = unsafeCoerce fect
runList (r :&: rs) fect = runList (unsafeCoerce rs) (runHandler r' fect')
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs'

\end{code}

Since we do not only want to run pure computations but also with monads
we need to run till the last one.

A different possibiltiy might be to encode the list length and based on that create something
that would however require natural numbers arithmetic

Might be possible to reduce the amount tof code by letting runList call runListM

lesson: don't try to combine -> substract


InitVect necessary anymore since we can get the InitVect effect using HandlerListM with injectivivity

\begin{code}
type family InitVect xs :: [k] where
   InitVect '[x] = '[]
   InitVect (t ': ts) = t ': (InitVect ts)

type family LastVect xs :: [k] where
   LastVect '[x] = '[x]
   LastVect (t ': ts) = LastVect ts
\end{code}

LastVect might be replacible by LastMember, but it is not a drop in replacement, and seems to need (unsafe) coercing that
is more cumbersome that the (LastVect effs ~ '[m]) coercing we do now.
-> Not the case, LastMember is only a type level computation using class/instances. It has no result

\begin{code}
runHandlerM :: (LastVect effs ~ '[m], Monad m) => HVect (HandlerListM effs) -> Eff effs a -> m a
runHandlerM hl eff = runM (runListL hl eff)

runListL :: HVect (HandlerListM effs) -> Eff effs a -> Eff (LastVect effs) a
runListL HNil fect = unsafeCoerce fect
runListL (r :&: rs) fect = unsafeCoerce (runListL (unsafeCoerce rs) (runHandler r' fect'))
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs'
\end{code}
This runListSafe doesn't work since we cant signal to the compiler that hwne HNil that its type is HVect  '[]

runListSafe :: forall effs a. HVect (HandlerList effs a) -> Eff effs a -> Eff '[] a
runListSafe vec fect = case vec of
                         g@HNil -> _
                         (r :&: rs) -> _


Een run partial zoals hieronder werkt niet doordat we geen injective type families hebben die ook met LHS kunnen werken
injectivity of type C zoals omschreven op GHC trac.
runListPartial :: forall effs effs2 a . HVect (HandlerList effs a)  -> Eff (Append effs effs2) a -> Eff effs2 a
runListPartial = _

Mogelijk alternative zou kunnen zijn gebruik maken van "split" Split effs eff2 ceff
Waar effs ++ effs2 = ceff

runListPartial :: Split effs effs ceff -> HVect (HandlerList effs a)  -> Eff (Append effs effs2) a -> Eff effs2 a

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

\end{code}
testRun2 :: Int
testRun2 = run $ runHeadList rsl _
  where rsl = (FR.runReader 5) :&: sl
        sl = (FS.evalState 3) :&: Nil

\begin{code}
test1Run3 :: Int
test1Run3 = run (runList ((Handler (FS.evalState 3)) :&: HNil) test1)

test2Run3 :: Int
test2Run3 = run (runList (Handler (FR.runReader 5) :&: HNil) test2)

testRun3 :: Int
testRun3 = run (runList (Handler (FR.runReader 5) :&: Handler (FS.evalState 3) :&: HNil) test)
\end{code}

Make handlerlist be non-empty such that it can be injective.
This is used to be able to work with SubLists.
\begin{code}
type family HandlerList effs = result | result -> effs where
  HandlerList '[] = '[]
  HandlerList (eff ': effs) = (Handler eff effs) ': HandlerList effs
\end{code}

We instroduce an extra list for handlers such that we get one entry less, but that this last
entry is still recorded in the effects lists. That is why Handerlist in combination with InitVect didn't work
\begin{code}
type family HandlerListM effs = result | result -> effs where
  HandlerListM '[eff, m] = '[Handler eff '[m]]
  HandlerListM (eff ': effs) = (Handler eff effs) ': HandlerListM effs
\end{code}

HVectElim looks interesting just do not know how to apply it

Toch een data family nodig?

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
testIORun2 = runHandlerM (Handler runConsole' :&: HNil) testIO
\end{code}

Having a type family flatten might drop the need for having to split interpreters. Is het echter voldoende om te weten
dat wanneer alles heeft gedraait alles weg is? Lijkt me wel voldoende, maar moet wel even oppassen.
HandlerList type family hoeft niet worden aangepast. Er moet alleen een flatten worden uitgevoerd of de eff lijst.

Encode the encoding also in the sublistrep?
Try somethign like SNatRep

\begin{code}
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

\begin{code}
extractHVect' :: SubList xs ys -> HVect ys -> HVect xs
extractHVect' Base r = HNil
extractHVect' (Keep sl) (r :&: rs) = r :&: (extractHVect' sl rs)
extractHVect' (Drop sl) (r :&: rs) = extractHVect' sl rs

extractHVect :: (SubListRep xs ys) => HVect ys -> HVect xs
extractHVect = extractHVect' (getSubList)
\end{code}

Gaat alleemaal niet niet meer doordat we extractHVect hebben gemaakt i.p.v. extractHandler
TODO: refactor de unsafeCoerce handler

Mooier zou wellicht zijn SubList s r i.p.v. onderstaande, op deze manier zijn er wel minder unsafeCoerce nodig
Anders zou de Base en Keep situatie nog een extra unsafeCoerce nodig hebben.

Voeg constraint equality toe. Op die manier moet het volgens mij lukken om de sublist eis die er nu is voor extractHandler' om te zetten naar (SubList s r)

De definitie van SubList gegeven zoals hierboven is niet informatierijk genoeg dat  de compiler kan achterhalen
wat de 'eff' en 'effs' moet zijn voor een (Handler eff effs). Onderstaande sublist gebruiken vereist weer typeintype wat ik niet wil doen.

data SubListHL (xs :: HandlerList effs a) (ys :: HandlerList effs2 a) where
  Base :: SubListHL '[] '[]
  Keep :: SubListHL xs ys -> SubListHL ((Handler eff effs a) ': HandlerList effs)  ((Handler eff effs2 a) ': HandlerList effs2 a)
  Drop :: SubListHL xs ys -> SubListHL (HandlerList effs a)  ((Handler eff effs2 a) ': HandlerList effs2 a)

Can't drop the a unless we find a way to drop it in Handerlist a
\begin{code}

extractHandler :: SubList (HandlerList s) (HandlerList r) -> HVect (HandlerList r) -> HVect (HandlerList s)
extractHandler Base r = HNil
extractHandler (Keep sl) (r :&: rs) = r :&: unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))
extractHandler (Drop sl) (r :&: rs) = unsafeCoerce (extractHandler (unsafeCoerce sl) (unsafeCoerce rs))


class SNatRep n where
    getSNat :: SNat n

instance SNatRep 'Zero where
    getSNat = SZero

instance SNatRep n => SNatRep ('Succ n) where
    getSNat = SSucc getSNat

--extractHandlersC ::
--  (SNatRep n,
--   ss ~ HandlerList s,
--   rs ~ HandlerList r,
--   tss ~ TakeVect n (HandlerList s),
--   dss ~ DropVect n (HandlerList s),
--   AppendVect tss dss ~ ss,
--   LenVect tss ~ n,
--   SubListRep ss rs) =>
--  HVect rs -> (HVect tss, HVect dss)
--extractHandlersC ss = splitVectB getSNat (extractHandler ss)

type family LTE n m where
  LTE Zero Zero = 'True
  LTE Zero (Succ Zero) = 'True
  LTE n Zero = 'False
  LTE (Succ n) (Succ m) = LTE n m

extractHandlers :: forall n s r m ss dss tss rs.
  (ss ~ HandlerList s,
   rs ~ HandlerList r,
   tss ~ TakeVect n ss,
   dss ~ DropVect n ss,
   LenVect ss ~ m) =>
  SNat n -> SubList ss rs -> HVect rs -> (HVect tss, HVect dss)
extractHandlers n sl rs = splitVectB n ss
  where ss :: HVect ss
        ss = extractHandler sl rs

--extractHandlersM ::
--  (SNatRep n,
--   ss ~ HandlerList s,
--   rs ~ HandlerList r,
--   tss ~ TakeVect n ss,
--   dss ~ DropVect n ss,
--   AppendVect tss dss ~ ss,
--   LenVect tss ~ n,
--   LenVect ss ~ m,
--   LTE n m ~ 'True,
--   SubListRep ss rs) =>
--  SNat n -> SubList ss rs -> HVect rs -> (HVect tss, HVect dss)
--extractHandlersM n sl ss = splitVectB n (extractHandler ss)

type family AppendVect xs ys where
  AppendVect '[] bs = bs
  AppendVect (a ': as) bs = a ': (AppendVect as bs)

type family LenVect xs where
  LenVect '[] = Zero
  LenVect (t ': ts) = Succ (LenVect ts)

\end{code}

Lesson: keep your steps as simple as possible. That way you might be able to use types without coercing
Lesson: You don't have to coerce if a type synonym suffices.
Lesson: Also allowed me to drop the polykinds extention

\begin{code}

\end{code}

\begin{code}
type PID = DP.ProcessId
\end{code}

En het bevat de argument waar je mee wilt werken. Het enige dat het nog mist is de effect

TODO add lift IO

For now we assume that Proc inside proc is not allowed.

Use the transitivity property of sublist

\begin{code}
data Proc (r :: [* -> *]) a where
  Spawn ::  (fs ~ FlattenProc s, ss ~ TillProc s, n ~ LenVect ss) => SNat n -> SubList fs r -> Eff s () -> Proc r PID
  Call ::  (fs ~ FlattenProc s, ss ~ TillProc s, n ~ LenVect ss) => SNat n -> SubList fs r -> Eff s a -> Proc r a
  LiftIO :: IO a -> Proc r a
  Send  :: DS.Serializable a => PID -> a ->  Proc r ()
  Expect :: DS.Serializable a => Proc r a
\end{code}
Since we now have subset proof those are probably cheaper to pass. But lets first keep it at run instances.
-> Not going to work due to needing it when spawning

Probably it is always going to need a proc instance.
The thing is that we can give it an identity function or some lifter such that it can't do much

Probably better with reintepret? Than we can move Proc r as the last handler. Might make stuff easier.
\begin{code}
runProc :: HVect (HandlerList r) -> Eff '[Proc r] a -> Eff '[DP.Process] a
runProc hl effs = translate (\case
                                Spawn n sl eff -> proHandler (DP.spawnLocal . void) n (convertSublist sl) hl eff
                                Call n sl eff -> proHandler DP.callLocal n (convertSublist sl) hl eff
                                LiftIO io -> liftIO io
                                Send pid id -> DP.send pid id
                                Expect -> DP.expect) effs

\end{code}

The sublist encoding of s and r or the sublist of a function list on s and r are the same.
Therefore we can convert one in the other.
\begin{code}
convertSublist :: SubList s r -> SubList (HandlerList s) (HandlerList r)
convertSublist = unsafeCoerce
\end{code}

The freer-simple library has support for sending to arbitrary member or the last member when that member is a monad.
We need to be able to send to the last member, but don't require that the last member is a monad.
\begin{code}
sendL :: (LastMember l effs) => l a -> Eff effs a
sendL = send
\end{code}

\begin{code}
type family TillProc xs where
  TillProc '[Proc ks] = '[]
  TillProc '[] = '[]
  TillProc (x ': xs) = x ': (TillProc xs)

type family GetProcContent xs where
  GetProcContent '[Proc ks] = ks
  GetProcContent '[] = '[]
  GetProcContent (x ': xs) = GetProcContent xs


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

proHandler :: forall a b fs ss hr fhs n s ks r. (
   fs ~ FlattenProc s,
   ss ~ TillProc s,
   hr ~ HandlerList r,
   fhs ~ HandlerList fs,
   ks ~ GetProcContent s) =>
   (DP.Process a -> DP.Process b) -> SNat n -> SubList fhs hr -> HVect hr -> Eff s a -> DP.Process b
proHandler f n sl hl eff = f (runM $ runProc ks' (unsafeCoerce (runListL ss' eff)))
  where (ss, ks) = extractHandlers n sl hl
        ss' :: HVect (HandlerListM s)
        ss' =  unsafeCoerce ss
        ks' :: HVect (HandlerList ks)
        ks' =  unsafeCoerce ks

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

Probably need to rewrite the GADT slightly to make subproc operations easier
Looks like the sublist proof could still just work rather than passing Handerlist explicit
-> Ask could probably not be completely list since we need to make sub procs.

Do something with rebindable do to keep the syntax minimalistic.

Subtype, zal ook weer iets van sublist worden.

Tests for the cases we want to support with proc

Maybe we can drop the proxy with something else, or wrap it in some kind of gadt

Adding a flatten for s will probably suffice

\begin{code}
hh :: Eff '[FR.Reader Int] Int
hh = FR.ask

gh :: Eff '[FR.Reader String] String
gh = FR.ask

\end{code}

Currently function that what to use IO functionality need to be explicitely augmented with Proc '[].
This could be fixed by introducing an spawnIO/callIO functions or by going through the Union (The effs) which takes the content of the last entry
and puts a DP.liftIO before it.

liftIO :: (LastMember IO s, ss ~ InitVect effs, r ~ AppendVect '[Proc '[]]) => Eff s a -> Eff r a

\begin{code}
f :: Eff [FR.Reader String, FR.Reader Int, Proc '[]] ()
f = do s <- FR.ask @String
       i <- FR.ask @Int
       sendIO $ putStrLn (s ++ show i)
\end{code}
Using implicitparameters does make it easier to change. We should however add defaultsignatures such that the first is
choosen by default

De volgende instanties hebben last van overlapping instances
Of stap over dat Proc de laatste moet zijn in de chain.

Iemand kan dan meerdere procs hebben, maar daar zul je niet echt heel veel last van hebben.

Misschien is type application ook wel te gebruiken?
Je kunt alleen maar substractive synthesis niet additive synthesis.
Dus als je iets echt wilt moet het of vooraan, of achteraan staan.
als nog kun je met commando's wel zeggen dat proc niet perse achteraan hoeft te staan?
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

i's type should still be a valid instance it just isn't that useful.
The code shouldn't work

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

\begin{code}
type family FlattenProc xs where
  FlattenProc '[Proc r] = r
  FlattenProc '[] = '[]
  FlattenProc (x ': xs) = x ': FlattenProc xs
\end{code}

Wrap constraints into a type, and possible use a type family such that a single constraint can be used?

Nodig is een lift into

De processes zullen van drie soorten zijn nadat alle handler zijn uitgevoerd:
- puur -> Eff '[] a. Deze zijn in proc te krijgen met raise
- IO -> Eff '[IO] a
- Process -> Eff '[Process] a of Eff '[Proc r] a

Van Eff '[IO] a naar Eff '[Process] a is een translate met liftio
Van Eff '[IO] a naar Eff ''

Stel we vereisen niet/gaan er van uit dat Proc r niet nog een Proc bevalt (Proc '[...., Proc]')
Dan is Handler r een lijst van alle handlers die we mogen gebruiken.

Gezien we runListM kunnen gebruiken willen we dus eigenlijk
InitVect s als handlerlist

Dit is wat we willen hebben
createHandler (LastMember (Proc k) s) (SubListRep k r) => Handerlist r -> Handerlist s

Waar dus de laatste entry van Handerlist s -> Handler (Proc k) '[]

easy is

createHandler (LastMember (Proc k) s) (SubListRep k r) => Handerlist r -> Handerlist s


Alle handlers in r mogen gebruikt worden.
Alle handeler in r, die nog niet voor de handlers van k zijn ge

Als je weet dat s' ++ k in zijn geheel een sublist is van r, dan zitten alle benodigde handlers in r

takevect probably needs to be coerced because we can't compare functions.
Or we add a Eq type for Handler which is not really equal.

Here we use strict take and drops, since that is what we need and it helps with type inference
\begin{code}
type family SplitAtVect n xs where
  SplitAtVect n xs = (HVect (TakeVect n xs), HVect (DropVect n xs))

splitVect :: SNat n -> HVect xs -> SplitAtVect n xs
splitVect n xs = (takeVect n xs, dropVect n xs)

type family TakeVect n xs where
  TakeVect Zero xs = '[]
  TakeVect (Succ m) (x ': xs) = x ': TakeVect m xs

type family DropVect n xs where
  DropVect Zero xs = xs
  DropVect (Succ m) (x ': xs) = DropVect m xs

type family SplitVect n xs where
  SplitVect n xs = '(TakeVect n xs, DropVect n xs)

takeVect :: SNat n -> HVect xs -> HVect (TakeVect n xs)
takeVect SZero xs = HNil
takeVect (SSucc m) (r :&: rs) = r :&: (takeVect m rs)

dropVect :: SNat n -> HVect xs -> HVect (DropVect n xs)
dropVect SZero xs = xs
dropVect (SSucc m) (r :&: rs) = dropVect m rs


splitVectB :: (DropVect n ss ~ ks, TakeVect n ss ~ ss') => SNat n -> HVect ss -> (HVect ss', HVect ks)
splitVectB n xs = splitVect n xs
\end{code}

Voeg type family toe voor partial application

type family HandlerListPartial effs effs2 where
