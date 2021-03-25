module Bintree ( buildTree, sumTree, printTree ) where

import Gibbon.Prim

--------------------------------------------------------------------------------

leafTag :: Tag
{-# INLINE leafTag #-}
leafTag = 0

nodeTag :: Tag
{-# INLINE nodeTag #-}
nodeTag = 1

buildTree :: Cursor a -> Int -> IO (Cursor a)
buildTree outcur n =
  case n of
    0 -> do !outcur1 <- writeTag outcur leafTag
            !outcur2 <- writeInt64 outcur1 1
            pure $! outcur2
    _ -> do !outcur1 <- writeTag outcur nodeTag
            !outcur2 <- buildTree outcur1 (n-1)
            !outcur3 <- buildTree outcur2 (n-1)
            pure $! outcur3

sumTree :: Cursor a -> IO (Int64, Cursor a)
sumTree !incur = do
    (!tag, incur1) <- readTag incur
    if tag `eqTag` leafTag
        then do (!i,incur2) <- {-# SCC sumLeaf #-} readInt64 incur1
                pure $! (i,incur2)
        else if tag `eqTag` nodeTag
                 then do (!i,incur2) <- {-# SCC sumLeft #-} sumTree incur1
                         (!j,incur3) <- {-# SCC sumRight #-} sumTree incur2
                         pure $! (i+j,incur3)
                 else error ("sumTree: unknown tag " ++ show tag)

printTree :: Cursor a -> IO (Cursor a)
printTree incur = do
    (tag, incur1) <- readTag incur
    if tag `eqTag` leafTag
        then do (i, incur2) <- readInt64 incur1
                putStr ("(Leaf " ++ show i ++ ")")
                pure incur2
        else if tag `eqTag` nodeTag
                 then do putStr ("(Node ")
                         incur2 <- printTree incur1
                         putStr (" ")
                         incur3 <- printTree incur2
                         putStr (")")
                         pure incur3
                 else error ("printTree: unknown tag " ++ show tag)
