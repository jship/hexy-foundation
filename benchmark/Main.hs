import Hexy

import Criterion.Main
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Numeric (showHex)
import Text.Printf (printf)

main :: IO ()
main = defaultMain
  [ bgroup "Int"
    [ bench "printf"   $ nf (printf "%08x" :: Int -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Int)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Int)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Int)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Int)
    ]
  , bgroup "Int8"
    [ bench "printf"   $ nf (printf "%08x" :: Int8 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int8)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Int8)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Int8)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Int8)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Int8)
    ]
  , bgroup "Int16"
    [ bench "printf"   $ nf (printf "%08x" :: Int16 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int16)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Int16)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Int16)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Int16)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Int16)
    ]
  , bgroup "Int32"
    [ bench "printf"   $ nf (printf "%08x" :: Int32 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int32)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Int32)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Int32)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Int32)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Int32)
    ]
  , bgroup "Int64"
    [ bench "printf"   $ nf (printf "%08x" :: Int64 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Int64)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Int64)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Int64)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Int64)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Int64)
    ]
  , bgroup "Word"
    [ bench "printf"   $ nf (printf "%08x" :: Word -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Word)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Word)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Word)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Word)
    ]
  , bgroup "Word8"
    [ bench "printf"   $ nf (printf "%08x" :: Word8 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word8)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Word8)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Word8)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Word8)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Word8)
    ]
  , bgroup "Word16"
    [ bench "printf"   $ nf (printf "%08x" :: Word16 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word16)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Word16)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Word16)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Word16)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Word16)
    ]
  , bgroup "Word32"
    [ bench "printf"   $ nf (printf "%08x" :: Word32 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word32)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Word32)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Word32)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Word32)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Word32)
    ]
  , bgroup "Word64"
    [ bench "printf"   $ nf (printf "%08x" :: Word64 -> String) 0x1f
    , bench "showHex"  $ nf (showHex (0x1f :: Word64)) ""
    , bench "xshow"    $ whnf xshow    (0x1f :: Word64)
    , bench "xshowp"   $ whnf xshowp   (0x1f :: Word64)
    , bench "xshowu"   $ whnf xshowu   (0x1f :: Word64)
    , bench "xshowpu"  $ whnf xshowpu  (0x1f :: Word64)
    ]
  ]
