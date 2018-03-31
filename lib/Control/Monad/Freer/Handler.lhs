\begin{code}
{-# language KindSignatures, DataKinds, TypeOperators #-}
\end{code}
To hide information about handlers.
\begin{code}
{-# language RankNTypes #-}
\end{code}
\begin{code}
{-# language TypeFamilies, TypeFamilyDependencies #-}
\end{code}
Necessary to guide information about constraints in extractHandlers.
\begin{code}
{-# language ScopedTypeVariables #-}
\end{code}
\begin{code}
module Control.Monad.Freer.Handler (extractHandler, extractHandlers, Handler (..), HandlerList, HandlerListL, runList, runListL, runHandler, runHandlers, runHandlersM ) where

import Control.Monad.Freer
import Data.HVect
import Data.HVect.Utils
import Unsafe.Coerce
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
