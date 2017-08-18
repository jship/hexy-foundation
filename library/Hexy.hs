module Hexy
  ( HexShow(..)
  ) where

import Hexy.Internal (showHexTextLower, showHexTextUpper, dropHexPrefix)

import Data.Int (Int, Int16, Int32, Int64, Int8)
import qualified Foundation
import Data.Word (Word, Word8, Word16, Word32, Word64)

class HexShow a where
  xshow :: a -> Foundation.String
  xshow = dropHexPrefix . xshowp

  xshowp :: a -> Foundation.String

  xshowu :: a -> Foundation.String
  xshowu = dropHexPrefix . xshowpu

  xshowpu :: a -> Foundation.String

  {-# MINIMAL xshowp, xshowpu #-}

instance HexShow Int where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int8 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int16 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int32 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Int64 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word8 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word16 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word32 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper

instance HexShow Word64 where
  xshowp  = showHexTextLower
  xshowpu = showHexTextUpper
