{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module EIO.TypeErrors
  (DisallowUnhandledExceptions)
   where

import Data.Kind (Type, Constraint)
import GHC.TypeLits

type family DisallowUnhandledExceptions (excepts :: [Type]) :: Constraint where
    DisallowUnhandledExceptions '[]     = ()
    DisallowUnhandledExceptions excepts =
      TypeError
        (     'Text "The 'runEIO' handler requires that all exceptions in 'EIO' to be handled."
        ':$$: 'Text "The action 'runEIO' is applied to throws the following unhandled exceptions:"
        ':$$: ShowTypeList excepts
        )

type family ShowTypeList (xs :: [Type]) :: ErrorMessage where
    ShowTypeList '[] = 'Text ""
    ShowTypeList (x ': xs) = 'Text "  • " ':<>: ('ShowType x) ':$$: (ShowTypeList xs)
