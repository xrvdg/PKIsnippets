\begin{code}
  module Process (
    ProcessT,
    PProcessT
  )
  where

  import Control.Monad.Reader
  import Control.Distributed.Process
\end{code}
TODO: This can probably be put somewhere under the distributed process hierarchy.

We introduce our own process type which is augmented with the information it is allow to use.
This such that we can use the types to show what kind of operations are allowed.

Preferably we blacklist rather than whitelist what is allowed but we want to convey more information in the types themselves and have helper functions which restrict the state that is carried and use phantom type to distinguise.

\begin{code}
  -- | A type that wraps process in a ReaderT where 'a' is the information it carries.
  type ProcessT a = ReaderT a Process

  -- | Add a phantom type to ProcessT such that we can restrict the kind of ProcessT that is given to a function.
  type PProcessT b a n = ProcessT a n

\end{code}
