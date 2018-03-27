To allow type level lists
\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
\begin{code}
{-# language RankNTypes #-}
\end{code}
To allow the definition of effects
\begin{code}
{-# language GADTs #-}
\end{code}
\begin{code}
{-# language TypeFamilies, TypeFamilyDependencies #-}
\end{code}
Required for gets
\begin{code}
{-# language TypeApplications #-}
\end{code}

For the IO test
\begin{code}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
\end{code}

For SubListRep
\begin{code}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language ConstraintKinds #-}
\end{code}

\begin{code}
module P (testRun) where
import Control.Monad.Freer
import qualified Control.Monad.Freer.Internal as FI
import qualified Control.Monad.Freer.Reader as FR
import qualified Control.Monad.Freer.State as FS
import Data.HVect as HV
import Data.OpenUnion ((:++:))
import Control.Distributed.Process (ProcessId, liftIO, Process, spawnLocal)
import Data.Proxy
import Unsafe.Coerce
import Debug.Trace
import Control.Monad.IO.Class
\end{code}

For the IO tests
\begin{code}
import System.Exit hiding (ExitCode(ExitSuccess))
\end{code}

\begin{code}
type PID = ProcessId
\end{code}

En het bevat de argument waar je mee wilt werken. Het enige dat het nog mist is de effect

\begin{code}
data RunList (r :: [* -> *])
data Proc r a where
  Spawn ::   RunList r' -> Eff r' () -> Proc r PID
  Call ::   RunList r' -> Eff r' () -> Proc r PID
  Send  :: a -> PID ->  Proc r a
  Expect :: Proc r a
\end{code}

\begin{code}

runPure :: HVect '[] -> Eff '[] a -> a
runPure HNil = run

type Handler b effs a = Eff (b ': effs) a -> Eff effs a

-- data HandlerM b effs a n = (LastMember n effs, Monad n) => HandlerM {runHandlerM :: Eff (b ': effs) a -> Eff effs a}

\end{code}
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

runHandler :: HVect (HandlerList effs a) -> Eff effs a -> a
runHandler hl eff = run (runList hl eff)

runList :: HVect (HandlerList effs a) -> Eff effs a -> Eff '[] a
runList HNil fect = unsafeCoerce fect
runList (r :&: rs) fect = runList (unsafeCoerce rs) (r' fect')
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs' a

\end{code}

Since we do not only want to run pure computations but also with monads
we need to run till the last one.

A different possibiltiy might be to encode the list length and based on that create something
that would however require natural numbers arithmetic

Might be possible to reduce the amount tof code by letting runList call runListM

lesson: don't try to combine -> substract
\begin{code}

type family InitVect xs :: [* -> *] where
   InitVect '[x] = '[]
   InitVect (t ': ts) = t ': (InitVect ts)

type family LastVect xs :: [* -> *] where
   LastVect '[x] = '[x]
   LastVect (t ': ts) = LastVect ts

runHandlerM :: (LastVect effs ~ '[m], Monad m) => HVect (HandlerListM effs a) -> Eff effs a -> m a
runHandlerM hl eff = runM (runListM hl eff)

runListM :: HVect (HandlerListM effs a) -> Eff effs a -> Eff (LastVect effs) a
runListM HNil fect = unsafeCoerce fect
runListM (r :&: rs) fect = unsafeCoerce (runListM (unsafeCoerce rs) (r' fect'))
  where fect' = (unsafeCoerce fect) :: Eff (eff' ': effs') a
        r' = (unsafeCoerce r) :: Handler eff' effs' a
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
test1Run3 = run (runList ((FS.evalState 3) :&: HNil) test1)

test2Run3 :: Int
test2Run3 = run (runList ((FR.runReader 5) :&: HNil) test2)

testRun3 :: Int
testRun3 = run (runList ((FR.runReader 5) :&: (FS.evalState 3) :&: HNil) test)
\end{code}

Make handlerlist be non-empty such that it can be injective.
This is used to be able to work with SubLists.
\begin{code}
type family HandlerList effs a = result | result -> effs a where
  HandlerList '[eff] a = '[(Handler eff '[] a)]
  HandlerList (eff ': effs) a = (Handler eff effs a) ': HandlerList effs a
\end{code}

We instroduce an extra list for handlers such that we get one entry less, but that this last
entry is still recorded in the effects lists. That is why Handerlist in combination with InitVect didn't work
\begin{code}
type family HandlerListM effs a = result | result -> effs a where
  HandlerListM '[eff, m] a = '[(Handler eff '[m] a)]
  HandlerListM (eff ': effs) a = (Handler eff effs a) ': HandlerListM effs a
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

testIORun2 :: Eff '[IO] ()
testIORun2 = runListM (runConsole' :&: HNil) testIO
\end{code}

Having a type family flatten might drop the need for having to split interpreters. Is het echter voldoende om te weten
dat wanneer alles heeft gedraait alles weg is? Lijkt me wel voldoende, maar moet wel even oppassen.
HandlerList type family hoeft niet worden aangepast. Er moet alleen een flatten worden uitgevoerd of de eff lijst.

\begin{code}
data SubList xs ys where
  Base :: SubList '[] '[]
  Keep :: SubList xs ys -> SubList (x ': xs) (x ': ys)
  Drop :: SubList xs ys -> SubList xs (y ': ys)

class SubListRep xs ys where
  getSubList :: SubList xs ys

instance SubListRep '[] '[] where
  getSubList = Base

instance SubListRep xs ys => SubListRep (x ': xs) (x ': ys) where
  getSubList = Keep getSubList

instance SubListRep xs ys => SubListRep xs (y ': ys) where
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
type SubListL s r a = (SubListRep (HandlerList s a) (HandlerList r a))
extractHandler :: SubListL s r a => HVect (HandlerList r a) -> HVect (HandlerList s a)
extractHandler = extractHVect
\end{code}

Lesson: keep your steps as simple as possible. That way you might be able to use types without coercing
Lesson: You don't have to coerce if a type synonym suffices.
Lesson: Also allowed me to drop the polykinds extention

\begin{code}

\end{code}
