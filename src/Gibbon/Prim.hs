{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE LinearTypes          #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs         #-}

module Gibbon.Prim
    ( Region, Cursor, Tag, Int64
    , eqTag, allocRegion, readTag, writeTag, readInt64, writeInt64
    , sizeofTag, sizeofInt64, bumpCur
    ) where

import GHC.Ptr               ( Ptr(..) )
import GHC.Base              ( IO(..), plusAddr#
                             , readInt8OffAddr#, readInt64OffAddr#
                             , writeInt8OffAddr#, writeInt64OffAddr#
                             )
import GHC.Int               ( Int8(..), Int64(..), eqInt8 )
import Foreign.Marshal.Alloc ( mallocBytes )
import GHC.IO                ( unsafeDupablePerformIO )
import qualified Unsafe.Linear as Unsafe
import Prelude.Linear        ( Consumable(..), Ur(..), lseq )

-- import Foreign.Storable
-- import GHC.Ptr
-- import GHC.Int               ( Int(..))

--------------------------------------------------------------------------------

type Region a = Ptr a
type Cursor a = Ptr a
type Tag = Int8

instance Consumable (Cursor a) where
    consume :: Cursor a %1-> ()
    consume ptr = Unsafe.toLinear f ptr
        where
          f _ = ()

eqTag :: Tag -> Tag -> Bool
{-# INLINE eqTag #-}
eqTag = eqInt8

allocRegion :: Int -> IO (Region a)
{-# INLINE allocRegion #-}
allocRegion = mallocBytes

readTag :: Cursor a %1-> (Ur Tag, Cursor a)
{-# INLINE readTag #-}
readTag ptr =
    Unsafe.toLinear f ptr
  where
    f (Ptr addr) = unsafeDupablePerformIO $ IO $ \s ->
                       case readInt8OffAddr# addr 0# s of
                           (# s2, x #) -> (# s2, (Ur (I8# x), Ptr (plusAddr# addr 1#)) #)
    -- let !v = unsafeDupablePerformIO (peek (Ptr addr))
    -- in (v, Ptr addr `plusPtr` 1)

writeTag :: Cursor a %1-> Tag %1-> Cursor a
{-# INLINE writeTag #-}
writeTag ptr val =
    Unsafe.toLinear2 f ptr val
  where
    f (Ptr addr) (I8# t) =
        unsafeDupablePerformIO $ IO $ \s ->
            case writeInt8OffAddr# addr 0# t s of
                s2 -> (# s2, Ptr (plusAddr# addr 1#) #)
    -- unsafeDupablePerformIO (poke (Ptr addr) (I8# t)) `seq`
    --    (Ptr addr `plusPtr` 1)


readInt64 :: Cursor a %1-> (Ur Int64, Cursor a)
{-# INLINE readInt64 #-}
readInt64 ptr =
    Unsafe.toLinear f ptr
  where
    f (Ptr addr) =
        unsafeDupablePerformIO $ IO $ \s ->
            case readInt64OffAddr# addr 0# s of
                (# s2, x #) -> (# s2, (Ur (I64# x), Ptr (plusAddr# addr 8#)) #)
    -- let !v = unsafeDupablePerformIO (peek (Ptr addr))
    -- in (v, Ptr addr `plusPtr` 8)

writeInt64 :: Cursor a %1-> Int64 -> Cursor a
{-# INLINE writeInt64 #-}
writeInt64 ptr val =
    Unsafe.toLinear2 f ptr val
  where
    f (Ptr addr) (I64# i) =
        unsafeDupablePerformIO $ IO $ \s ->
            case writeInt64OffAddr# addr 0# i s of
                s2 -> (# s2, Ptr (plusAddr# addr 8#) #)
    -- unsafeDupablePerformIO (poke (Ptr addr) (I64# i)) `seq`
    --    (Ptr addr `plusPtr` 8)

sizeofTag :: Int64
{-# INLINE sizeofTag #-}
sizeofTag = I64# 1#

-- sizeofTag# :: Int#
-- {-# INLINE sizeofTag# #-}
-- sizeofTag# = 1#

sizeofInt64 :: Int64
{-# INLINE sizeofInt64 #-}
sizeofInt64 = I64# 8#

bumpCur :: Cursor a -> Int64 -> Cursor a
{-# INLINE bumpCur #-}
bumpCur (Ptr addr) (I64# i) = Ptr (plusAddr# addr i)
