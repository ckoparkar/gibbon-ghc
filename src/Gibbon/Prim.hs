{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE UnboxedTuples    #-}

module Gibbon.Prim
    ( Region, Cursor, Tag, Int64
    , eqTag, sizeofTag, sizeofInt64, bumpCur
    , readTag, writeTag, readInt64, writeInt64
    , allocRegionIO, readTagIO, writeTagIO, readInt64IO, writeInt64IO
    ) where

import GHC.Ptr               ( Ptr(..) )
import GHC.Base              ( IO(..), plusAddr#
                             , readInt8OffAddr#, readInt64OffAddr#
                             , writeInt8OffAddr#, writeInt64OffAddr#
                             )
import GHC.Int               ( Int8(..), Int64(..), eqInt8 )
import Foreign.Marshal.Alloc ( mallocBytes )
import GHC.IO                ( unsafeDupablePerformIO )

-- import Foreign.Storable
-- import GHC.Ptr
-- import GHC.Int               ( Int(..))

--------------------------------------------------------------------------------

type Region a = Ptr a
type Cursor a = Ptr a
type Tag = Int8

eqTag :: Tag -> Tag -> Bool
{-# INLINE eqTag #-}
eqTag = eqInt8

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

--------------------------------------------------------------------------------

allocRegionIO :: Int -> IO (Region a)
{-# INLINE allocRegionIO #-}
allocRegionIO = mallocBytes

readTag :: Cursor a -> (Tag, Cursor a)
{-# INLINE readTag #-}
readTag = unsafeDupablePerformIO . readTagIO

readTagIO :: Cursor a -> IO (Tag, Cursor a)
{-# INLINE readTagIO #-}
readTagIO (Ptr addr) =
    IO $ \s ->
        case readInt8OffAddr# addr 0# s of
            (# s2, x #) -> (# s2, (I8# x, Ptr (plusAddr# addr 1#)) #)
    -- let !v = unsafeDupablePerformIO (peek (Ptr addr))
    -- in (v, Ptr addr `plusPtr` 1)

writeTag :: Cursor a -> Tag -> Cursor a
{-# INLINE writeTag #-}
writeTag cur tag = unsafeDupablePerformIO (writeTagIO cur tag)

writeTagIO :: Cursor a -> Tag -> IO (Cursor a)
{-# INLINE writeTagIO #-}
writeTagIO (Ptr addr) (I8# t) =
    IO $ \s ->
        case writeInt8OffAddr# addr 0# t s of
            s2 -> (# s2, Ptr (plusAddr# addr 1#) #)
    -- unsafeDupablePerformIO (poke (Ptr addr) (I8# t)) `seq`
    --    (Ptr addr `plusPtr` 1)

readInt64 :: Cursor a -> (Int64, Cursor a)
{-# INLINE readInt64 #-}
readInt64 = unsafeDupablePerformIO . readInt64IO

readInt64IO :: Cursor a -> IO (Int64, Cursor a)
{-# INLINE readInt64IO #-}
readInt64IO (Ptr addr) =
    IO $ \s ->
        case readInt64OffAddr# addr 0# s of
            (# s2, x #) -> (# s2, (I64# x, Ptr (plusAddr# addr 8#)) #)
    -- let !v = unsafeDupablePerformIO (peek (Ptr addr))
    -- in (v, Ptr addr `plusPtr` 8)

writeInt64 :: Cursor a -> Int64 -> Cursor a
{-# INLINE writeInt64 #-}
writeInt64 cur int = unsafeDupablePerformIO (writeInt64IO cur int)

writeInt64IO :: Cursor a -> Int64 -> IO (Cursor a)
{-# INLINE writeInt64IO #-}
writeInt64IO (Ptr addr) (I64# i) =
    IO $ \s ->
        case writeInt64OffAddr# addr 0# i s of
            s2 -> (# s2, Ptr (plusAddr# addr 8#) #)
    -- unsafeDupablePerformIO (poke (Ptr addr) (I64# i)) `seq`
    --    (Ptr addr `plusPtr` 8)
