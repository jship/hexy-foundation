{-# LANGUAGE BangPatterns #-}

module Hexy.Internal where

import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
import qualified Data.Char as Char
import Data.Int (Int, Int8, Int16, Int32, Int64)
import qualified Foundation
import qualified Foundation.Array
import qualified Foundation.Collection
import qualified Foundation.String
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable as Storable

showHexTextLower :: (Integral a, Show a, Storable a) => a -> Foundation.String
{-# SPECIALIZE showHexTextLower :: Int -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Int8 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Int16 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Int32 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Int64 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Word -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Word8 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Word16 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Word32 -> Foundation.String #-}
{-# SPECIALIZE showHexTextLower :: Word64 -> Foundation.String #-}
showHexTextLower = textShowIntAtBase 16 intToDigitLower

showHexTextUpper :: (Integral a, Show a, Storable a) => a -> Foundation.String
{-# SPECIALIZE showHexTextUpper :: Int -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Int8 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Int16 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Int32 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Int64 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Word -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Word8 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Word16 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Word32 -> Foundation.String #-}
{-# SPECIALIZE showHexTextUpper :: Word64 -> Foundation.String #-}
showHexTextUpper = textShowIntAtBase 16 intToDigitUpper

textShowIntAtBase :: (Integral a, Show a, Storable a) => a -> (Int -> Char) -> a -> Foundation.String
{-# SPECIALIZE textShowIntAtBase :: Int -> (Int -> Char) -> Int -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Int8 -> (Int -> Char) -> Int8 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Int16 -> (Int -> Char) -> Int16 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Int32 -> (Int -> Char) -> Int32 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Int64 -> (Int -> Char) -> Int64 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Word -> (Int -> Char) -> Word -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Word8 -> (Int -> Char) -> Word8 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Word16 -> (Int -> Char) -> Word16 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Word32 -> (Int -> Char) -> Word32 -> Foundation.String #-}
{-# SPECIALIZE textShowIntAtBase :: Word64 -> (Int -> Char) -> Word64 -> Foundation.String #-}
textShowIntAtBase base toChr n0
  | base <= 1 = errorWithoutStackTrace ("Hexy.Internal.textShowIntAtBase: applied to unsupported base " ++ show base)
  | n0   <  0 = errorWithoutStackTrace ("Hexy.Internal.textShowIntAtBase: applied to negative number " ++ show n0)
  | otherwise = ST.runST $ do
      let !size = 2 + (2 * Storable.sizeOf n0)
      mutableBuffer <- Foundation.Collection.mutNew (Foundation.CountOf size) :: ST s (Foundation.Array.MUArray Word8 s)
      let hexLoop i (n, d) = do
            i' <- unsafeWriteRev mutableBuffer i c
            case n of
              0 -> pure i'
              _ -> hexLoop i' (quotRem n base)
           where
            c = toChr $ fromIntegral d
      let zeroPadLoop i
            | i < 2 = pure i
            | otherwise = do
                i' <- unsafeWriteRev mutableBuffer i '0'
                zeroPadLoop i'
      j <- hexLoop (Foundation.Offset $ size - 1) (quotRem n0 base)
      k <- zeroPadLoop j
      l <- unsafeWriteRev mutableBuffer k 'x'
      _ <- unsafeWriteRev mutableBuffer l '0'
      immutableBuffer <- Foundation.Collection.unsafeFreeze mutableBuffer
      pure . Foundation.String.fromBytesUnsafe $ immutableBuffer

unsafeWriteRev :: Foundation.Array.MUArray Word8 s -> Foundation.Offset Word8 -> Char -> ST s (Foundation.Offset Word8)
unsafeWriteRev buffer i c = do
  Foundation.Collection.mutUnsafeWrite buffer i (fromIntegral . Char.ord $ c)
  pure (i - 1)

dropHexPrefix :: Foundation.String -> Foundation.String
dropHexPrefix = Foundation.drop 2

intToDigitLower :: Int -> Char
intToDigitLower i
  | i >=  0 && i <=  9 = Char.chr (fromIntegral $ Char.ord '0' + i)
  | i >= 10 && i <= 15 = Char.chr (fromIntegral $ Char.ord 'a' + i - 10)
  | otherwise = errorWithoutStackTrace ("Hexy.Internal.intToDigitLower: not a digit " ++ show i)

intToDigitUpper :: Int -> Char
intToDigitUpper i
  | i >=  0 && i <=  9 = Char.chr (fromIntegral $ Char.ord '0' + i)
  | i >= 10 && i <= 15 = Char.chr (fromIntegral $ Char.ord 'A' + i - 10)
  | otherwise = errorWithoutStackTrace ("Hexy.Internal.intToDigitUpper: not a digit " ++ show i)
