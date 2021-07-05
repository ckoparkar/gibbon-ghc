-- {-# LANGUAGE Strict      #-}
{-# LANGUAGE LinearTypes #-}

module LinearBintree ( buildTree, sumTree, printTree ) where

import Gibbon.LinearPrim
import Prelude.Linear ( (&), lseq, Ur(..) )
import qualified Unsafe.Linear as Unsafe

--------------------------------------------------------------------------------

leafTag :: Tag
{-# INLINE leafTag #-}
leafTag = 0

nodeTag :: Tag
{-# INLINE nodeTag #-}
nodeTag = 1

buildTree :: Cursor a %1-> Int -> Cursor a
buildTree !outcur !n =
  case n of
    0 -> writeTag outcur leafTag &
             \(!outcur1) ->
                 writeInt64 outcur1 1
    _ -> writeTag outcur nodeTag &
             \(!outcur1) ->
                 buildTree outcur1 (n-1) &
                     \(!outcur2) ->
                         buildTree outcur2 (n-1)

sumTree :: Cursor a %1-> (Int64, Cursor a)
sumTree !incur =
    readTag incur &
        \((Ur !tag), !incur1) ->
            if tag `eqTag` leafTag
            then readInt64 incur1 &
                     \(Ur !i, !incur2) -> (i,incur2)
            else if tag `eqTag` nodeTag
                 then sumTree incur1 &
                          \(!i,!incur2) ->
                              sumTree incur2 &
                                  \(!j,!incur3) ->
                                      (Unsafe.toLinear2 (+) i j, incur3)
                 else incur1 `lseq` (error ("sumTree: unknown tag " ++ show tag))

printTree :: Cursor a -> IO (Cursor a)
printTree incur = do
    let ((Ur tag), incur1) = readTag incur
    if tag `eqTag` leafTag
        then do let (Ur i, incur2) = readInt64 incur1
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
