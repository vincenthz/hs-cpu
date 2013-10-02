{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- |
-- Module      : System.Endian
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module System.Endian
    ( Endianness(..)
    , getSystemEndianness
    -- little endian to and from cpu
    , fromLE32
    , fromLE64
    , toLE32
    , toLE64
    -- big endian to and from cpu
    , fromBE32
    , fromBE64
    , toBE32
    , toBE64
    ) where

#include "MachDeps.h"

import Data.Bits.Extras
import Data.Word

-- | represent the CPU endianness
--
-- Big endian system stores bytes with the MSB as the first byte.
-- Little endian system stores bytes with the LSB as the first byte.
--
-- middle endian is purposely avoided.
data Endianness = LittleEndian
                | BigEndian
                deriving (Show,Eq)

-- | return the system endianness
getSystemEndianness :: Endianness
#ifdef WORDS_BIGENDIAN
getSystemEndianness = BigEndian
#else
getSystemEndianness = LittleEndian
#endif

-- | Convert from a big endian 64 bit value to the cpu endianness
fromBE64 :: Word64 -> Word64
fromBE64 = if getSystemEndianness == BigEndian then id else byteSwap

-- | Convert from a little endian 64 bit value to the cpu endianness
fromLE64 :: Word64 -> Word64
fromLE64 = if getSystemEndianness == LittleEndian then id else byteSwap

-- | Convert from a big endian 32 bit value to the cpu endianness
fromBE32 :: Word32 -> Word32
fromBE32 = if getSystemEndianness == BigEndian then id else byteSwap

-- | Convert from a little endian 32 bit value to the cpu endianness
fromLE32 :: Word32 -> Word32
fromLE32 = if getSystemEndianness == LittleEndian then id else byteSwap

-- | Convert a 64 bit value in cpu endianess to big endian
toBE64 :: Word64 -> Word64
toBE64 = fromBE64

-- | Convert a 64 bit value in cpu endianess to little endian
toLE64 :: Word64 -> Word64
toLE64 = fromLE64

-- | Convert a 32 bit value in cpu endianess to big endian
toBE32 :: Word32 -> Word32
toBE32 = fromBE32

-- | Convert a 32 bit value in cpu endianess to little endian
toLE32 :: Word32 -> Word32
toLE32 = fromLE32
