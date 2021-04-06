{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

IO with Exceptions tracked on the type-level
-}

module EIO
    ( -- * Main type
      EIO (..)
    , runEIO
      -- * Basic API
    , throw
    , catch
    , unsafeLiftIO
    , tryLiftIO
      -- * QualifieDo interface
    , return
    , (>>=)
    , (>>)
    ) where

import Prelude hiding (return, (>>), (>>=))

import Control.Exception (Exception, try)
import Data.Coerce (coerce)
import Data.Kind (Type)
import EIO.TypeErrors (DisallowUnhandledExceptions)

import qualified GHC.IO as IO
import qualified Prelude

-- $setup
-- >>> data MyErr = MyErr deriving (Show)
-- >>> instance Exception MyErr

{- | Main type for 'IO' that tracks exceptions on the
type-level. Simply wraps 'IO' and adds exceptions meta-information.

@since 0.0.0.0
-}
newtype EIO (exceptions :: [Type]) a = EIO
    { unEIO :: IO a
    } deriving newtype (Functor)

{- | Run the the 'EIO' main with exception tracking.


Usually used in the @main@ function like so:

@
__import__ EIO (EIO)
__import qualified__ EIO

main :: IO ()
main = EIO.'runEIO' safeMain

safeMain :: EIO '[] ()
safeMain = EIO.do
    ... your code ...
@

>>> :{
  runEIO $ EIO.do
    throw MyErr `catch` (\MyErr -> unsafeLiftIO $ putStrLn "handled error")
    unsafeLiftIO $ putStrLn "ran action"
>>> :}
handled error
ran action

>>> EIO.runEIO $ EIO.throw MyErr >> EIO.return ()
...
... • The 'runEIO' handler requires that all exceptions in 'EIO' to be handled.
...   The action 'runEIO' is applied to throws the following unhandled exceptions:
...     • MyErr
...

@since 0.0.0.0
-}
runEIO :: (DisallowUnhandledExceptions excepts) => EIO excepts () -> IO ()
runEIO = coerce


{- | Wrap a value into 'EIO' without throwing any exceptions.

@since 0.0.0.0
-}
return :: forall a . a -> EIO '[] a
return = coerce @(a -> IO a) Prelude.return

{- | Bind the value inside the first action to the second one and
combine thrown exceptions.

@since 0.0.0.0
-}
(>>=) :: forall e1 e2 a b . EIO e1 a -> (a -> EIO e2 b) -> EIO (e1 <> e2) b
(>>=) = coerce @(IO a -> (a -> IO b) -> IO b) (Prelude.>>=)

{- | Run two actions sequentially and combine thrown exceptions.

@since 0.0.0.0
-}
(>>) :: forall e1 e2 a b . EIO e1 a -> EIO e2 b -> EIO (e1 <> e2) b
(>>) = coerce @(IO a -> IO b -> IO b) (Prelude.>>)

type family (<>) (xs :: [Type]) (ys :: [Type]) :: [Type] where
    '[] <> ys = ys
    xs <> '[] = xs
    (x ': xs) <> ys = x ': (xs <> ys)

{- | Allows one to lift an IO action into EIO, but you are telling the compiler
that there are no exceptions in your IO action. The safety of this function is
contingent on the user keeping their promise of exception free code, which is why
this function is labelled as unsafe.

@since 0.0.1.1
-}
unsafeLiftIO :: IO a -> EIO '[] a
unsafeLiftIO = EIO


{- | A safer version of `unsafeLiftIO` this function first tries the action
and forces the caller to handle the exception purely before before proceeding
in EIO with a clean exception state.

@since 0.0.1.1
-}
tryLiftIO :: (Exception e) => IO a -> EIO '[] (Either e a)
tryLiftIO = EIO . try

{- | Throw exception.

@since 0.0.0.0
-}
throw :: forall e a . Exception e => e -> EIO '[ e ] a
throw = coerce @(e -> IO a) IO.throwIO

{- | Catch exception and remove it from the list of handled exceptions.

@since 0.0.0.0
-}
catch
    :: forall e a e1 e2
    .  Exception e
    => EIO e1 a
    -> (e -> EIO e2 a)
    -> EIO (Delete e e1 <> e2) a
catch = coerce @(IO a -> (e -> IO a) -> IO a) IO.catch

type family Delete (x :: Type) (xs :: [Type]) :: [Type] where
    Delete _ '[] = '[]
    Delete x (x ': xs) = Delete x xs
    Delete x (y ': xs) = y ': Delete x xs
