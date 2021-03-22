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
      -- * QualifieDo interface
    , return
    , (>>=)
    , (>>)
    ) where

import Prelude hiding (return, (>>), (>>=))

import Control.Exception (Exception)
import Data.Coerce (coerce)
import Data.Kind (Type)

import qualified GHC.IO as IO
import qualified Prelude


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

@since 0.0.0.0
-}
runEIO :: EIO '[] () -> IO ()
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
