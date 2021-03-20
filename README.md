# eio

[![GitHub CI](https://github.com/kowainik/eio/workflows/CI/badge.svg)](https://github.com/kowainik/eio/actions)
[![Hackage](https://img.shields.io/hackage/v/eio.svg?logo=haskell)](https://hackage.haskell.org/package/eio)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

IO with Exceptions tracked on the type-level.


__Note:__ The package is considered to be used with the `QualifiedDo` feature,
so hence the support only of GHC-9.0 and upper.

## Usage example

Since this is a literate haskell file, we need to specify all our language
extensions and imports up front.

```haskell
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
```

It is recomended to use `eio` library qualified, as it reimplements many
standard functions.

```haskell
import Control.Exception (Exception)
import EIO (EIO)

import qualified EIO
```

Let's also define our own exception to play with:

```haskell
data MyErr = MyErr
    deriving stock (Show)
    deriving anyclass (Exception)
```

The main function of our module will look like this:

```haskell
main :: IO ()
main = EIO.runEIO safeMain
```

Let's now write the safe main function that should not have any exceptions
pushed to the actual `main` function, as the list of exceptions on type level
should be empty.

This means, that if we throw the exception inside but don't handle it properly,
it won't type check, as the list of exceptions in `EIO` will contain at least
one element:

```idris
-- - Type error!
safeMainWrong :: EIO '[] ()
safeMainWrong = EIO.do
    EIO.throw MyErr
```

And the error will exactly point out to the fact that the empty list is expected, but got non-empty instead:

```idris
error:
    • Couldn't match type: '[MyErr]
                     with: '[]
      Expected: EIO '[] ()
        Actual: EIO '[MyErr] ()
    • In a stmt of a qualified 'do' block: EIO.throw MyErr
      In the expression: EIO.do EIO.throw MyErr
      In an equation for ‘safeMain’: safeMain = EIO.do EIO.throw MyErr
   |
xx |     EIO.throw MyErr
   |     ^^^^^^^^^^^^^^^
```

In order for it to type check, we need to handle each thrown exception properly,
so that you have an empty list in the end:

```haskell
safeMain :: EIO '[] ()
safeMain = EIO.do
    EIO.return ()
    EIO.throw MyErr `EIO.catch` (\MyErr -> EIO.return ())
```
