{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}

module Gibbon.IO where

import GHC.Base
import GHC.Types

--------------------------------------------------------------------------------

newtype IO# (r :: RuntimeRep) (a :: TYPE r) =
    IO# (State# RealWorld -> (# State# RealWorld, a #))

instance Functor (IO# (r :: RuntimeRep)) where
    fmap f x = x >>= (pure . f)

instance Applicative (IO# (r :: RuntimeRep)) where
    {-# INLINE pure #-}
    {-# INLINE (*>) #-}
    {-# INLINE liftA2 #-}
    pure  = returnIO#
    (*>)  = thenIO#
    (<*>) = ap
    liftA2 = liftM2

instance  Monad (IO# (r :: RuntimeRep)) where
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    (>>)      = (*>)
    (>>=)     = bindIO#

returnIO# :: a -> IO# a
returnIO# x = IO# (\ s -> (# s, x #))

bindIO# :: IO# a -> (a -> IO# b) -> IO# b
bindIO# (IO# m) k = IO# (\ s -> case m s of (# new_s, a #) -> unIO# (k a) new_s)

thenIO# :: IO# a -> IO# b -> IO# b
thenIO# (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO# k new_s)

unsafeDupablePerformIO# :: IO# a -> a
unsafeDupablePerformIO# (IO# m) = case runRW# m of (# _, a #) -> a
