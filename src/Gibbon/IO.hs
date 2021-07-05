{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE GADTs            #-}

module Gibbon.IO where

import GHC.Base
import GHC.Types

--------------------------------------------------------------------------------

newtype IO# (r :: RuntimeRep) (a :: TYPE r) =
    IO# (State# RealWorld -> (# State# RealWorld, a #))

-- instance Functor (IO# (r :: RuntimeRep)) where
--     fmap f x = x >>= (pure . f)

-- instance Applicative (IO# (r :: RuntimeRep)) where
--     {-# INLINE pure #-}
--     {-# INLINE (*>) #-}
--     {-# INLINE liftA2 #-}
--     pure  = returnIO#
--     (*>)  = thenIO#
--     (<*>) = ap
--     liftA2 = liftM2

-- instance  Monad (IO# (r :: RuntimeRep)) where
--     {-# INLINE (>>)   #-}
--     {-# INLINE (>>=)  #-}
--     (>>)      = (*>)
--     (>>=)     = bindIO#


unIO# :: forall r a. IO# r a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO# (IO# a) = a

returnIO# :: forall r a. a -> IO# r a
returnIO# x = IO# (\ s -> (# s, x #))

bindIO# :: forall r a b. IO# r a -> (a -> IO# r b) -> IO# r b
bindIO# (IO# m) k = IO# (\ s -> case m s of (# new_s, a #) -> unIO# (k a) new_s)

thenIO# :: forall r a b. IO# r a -> IO# r b -> IO# r b
thenIO# (IO# m) k = IO# (\ s -> case m s of (# new_s, _ #) -> unIO# k new_s)

-- unsafeDupablePerformIO# :: forall r a. (r ~ 'UnliftedRep) => IO# r a -> a
-- unsafeDupablePerformIO# (IO# m) = case runRW# m of (# _, a #) -> a
