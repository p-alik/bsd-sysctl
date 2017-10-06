{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- vim:filetype=haskell
-----------------------------------------------------------------------------
-- |
-- Module      :  System.BSD.Sysctl
-- Copyright   :  (c) Maxime Henrion 2009-2010
-- License     :  see LICENSE
--
-- Maintainer  :  mhenrion@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- This module allows access to the BSD sysctl(3) interface via the Haskell FFI.
--
-- Convenience functions to read and write the usual sysctl types are provided,
-- as well as more advanced functions to handle binary values given a suitable
-- 'Storable' instance.  It is also possible to retrieve data whose size changes
-- at runtime with the 'sysctlPeekArray' function.
--
-- On some platforms, there are sysctl nodes that accept parameters via
-- additional components in the OID (see for instance the \"kern.proc.pid\"
-- sysctl described in sysctl(3) on FreeBSD).  The 'sysctlNameToOidArgs'
-- function makes it easy to query such nodes as well.
--
-- Nodes may be queried either by their OID as a list of integers, by their
-- binary OID for maximum speed, or by their names on platforms that support it.
-------------------------------------------------------------------------------

#include <sys/param.h>
#include <sys/sysctl.h>

#if defined(__HADDOCK__) || (!defined(__linux__) && !defined(__OpenBSD__))
#define HAVE_SYSCTLNAMETOMIB
#endif

module System.BSD.Sysctl (
  -- * The data types
  SysctlKey,
  OID,

  -- * OID creation and extraction
#ifdef HAVE_SYSCTLNAMETOMIB
  sysctlNameToOid,  -- :: String -> IO OID
  sysctlNameToOidArgs,  -- :: String -> [#{type int}] -> IO OID
#endif
  sysctlPrepareOid, -- :: [#{type int}] -> IO OID
  sysctlExtractOid, -- :: OID -> IO [#{type int}]

  -- * Basic reading functions
  sysctlReadInt,  -- :: SysctlKey k => k -> IO #{type int}
  sysctlReadUInt, -- :: SysctlKey k => k -> IO #{type unsigned int}
  sysctlReadLong, -- :: SysctlKey k => k -> IO #{type long}
  sysctlReadULong,  -- :: SysctlKey k => k -> IO #{type unsigned long}
  sysctlReadQuad, -- :: SysctlKey k => k -> IO Int64
  sysctlReadUQuad,  -- :: SysctlKey k => k -> IO Word64
  sysctlReadString, -- :: SysctlKey k => k -> IO String

  -- * Advanced reading functions
  sysctlPeek,   -- :: forall k a. (SysctlKey k, Storable a) => k -> IO a
  sysctlPeekArray,  -- :: forall k a. (SysctlKey k, Storable a) => k -> IO [a]

  -- * Basic writing functions
  sysctlWriteInt, -- :: SysctlKey k => k -> #{type int} -> IO ()
  sysctlWriteUInt,  -- :: SysctlKey k => k -> #{type unsigned int} -> IO ()
  sysctlWriteLong,  -- :: SysctlKey k => k -> #{type long} -> IO ()
  sysctlWriteULong, -- :: SysctlKey k => k -> #{type unsigned long} -> IO ()
  sysctlWriteQuad,  -- :: SysctlKey k => k -> Int64 -> IO ()
  sysctlWriteUQuad, -- :: SysctlKey k => k -> Word64 -> IO ()
  sysctlWriteString,  -- :: SysctlKey k => k -> String -> IO ()

  -- * Advanced writing functions
  sysctlPoke    -- :: (SysctlKey k, Storable a) => k -> a -> IO ()
  ) where

import Control.Arrow (second)
import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr

-- | An efficient representation of a sysctl 'OID' for maximum performance.
data OID = OID {-# UNPACK #-} !(ForeignPtr CInt)
               {-# UNPACK #-} !CUInt

-- | The class of types that can be used to identify a sysctl node.
class SysctlKey k where
  withKey :: k -> (Ptr CInt -> CUInt -> IO a) -> IO a

instance SysctlKey OID where
  withKey (OID fp len) f = withForeignPtr fp (\ptr -> f ptr len)

instance SysctlKey [#{type int}] where
  withKey oid f = withArrayLen (map fromIntegral oid)
                               (\len ptr -> f ptr (fromIntegral len))

foreign import ccall unsafe "sysctl"
  c_sysctl :: Ptr CInt -> CUInt -> Ptr a -> Ptr CSize -> Ptr b -> CSize -> IO CInt

-- | Prepare an 'OID' for later use.
sysctlPrepareOid :: [#{type int}] -> IO OID
sysctlPrepareOid []  = error "sysctPrepareOid: empty list"
sysctlPrepareOid oid =
 do fp <- mallocForeignPtrArray len
    withForeignPtr fp (flip pokeArray (map fromIntegral oid))
    return (OID fp (fromIntegral len))
  where len = length oid

#ifdef HAVE_SYSCTLNAMETOMIB
-- Support for looking up sysctls by name.  This is not supported
-- on (at least) Linux and OpenBSD.
foreign import ccall unsafe "sysctlnametomib"
  c_sysctlnametomib :: CString -> Ptr CInt -> Ptr CSize -> IO CInt

-- | Get the 'OID' corresponding to a sysctl name.
sysctlNameToOid :: String -> IO OID
sysctlNameToOid name = sysctlNameToOidArgs name []

-- | Like 'sysctlNameToOid', but allows to provide a list of
-- additional integers to append to the OID, for specific sysctl
-- nodes that support parameters this way.
sysctlNameToOidArgs :: String -> [#{type int}] -> IO OID
sysctlNameToOidArgs name args =
  withCString name $ \cname -> do
    allocaArray (fromIntegral maxlen) $ \oid -> do
      alloca $ \sizePtr -> do
        poke sizePtr maxlen
        throwErrnoIfMinus1_ "sysctlnametomib"
          (c_sysctlnametomib cname oid sizePtr)
        nlen <- fromIntegral `fmap` peek sizePtr
        -- Copy to a new buffer to save space.
        let len = nlen + alen
        fp <- mallocForeignPtrArray len
        withForeignPtr fp $ \ptr -> do
          copyArray ptr oid nlen
          pokeArray (ptr `advancePtr` nlen) (map fromIntegral args)
        return (OID fp (fromIntegral len))
  where maxlen = #{const CTL_MAXNAME}
        alen   = length args

instance SysctlKey String where
  withKey name f = sysctlNameToOid name >>= flip withKey f
#endif

{-
-- This could be used to implement some form of type checking at runtime some
-- day, but the interface is undocumented and probably unportable though.
oidToType :: Ptr CInt -> CUInt -> IO (CUInt, String)
oidToType oid len =
  let len' = len + 2 in
    allocaArray (fromIntegral len') $ \oid' -> do
      poke oid' 0
      poke (oid' `advancePtr` 1) 4
      copyArray (oid' `advancePtr` 2) oid (fromIntegral len)
      allocaBytes defaultBufSize $ \buf ->
        sysctlRead oid' len' buf (fromIntegral defaultBufSize) $ \buf' _ -> do
           kind <- peek buf'
           fmt  <- peekCString (buf' `plusPtr` (sizeOf kind))
           return (kind, fmt)
  where defaultBufSize = 1024 -- as in FreeBSD's libc
-}

-- | Extract the list of integers contained in an 'OID'.
sysctlExtractOid :: OID -> IO [#{type int}]
sysctlExtractOid (OID fp len) =
  map fromIntegral `fmap` withForeignPtr fp (peekArray (fromIntegral len))

-- Base primitive for all reading operations.  Abstracts away the low-level C
-- machinery such as using a pointer to have multiple return values.
sysctlRead :: Ptr CInt -> CUInt -> Ptr a -> CSize -> (Ptr a -> CSize -> IO b) -> IO b
sysctlRead oid len buf size f =
  alloca $ \sizePtr -> do
    poke sizePtr size
    throwErrnoIfMinus1_ "sysctl"
      (c_sysctl oid len buf sizePtr nullPtr 0)
    realSize <- peek sizePtr
    f buf realSize

-- Call sysctl with a size set to 0 to retrieve the size of the object.
sysctlGetSize :: Ptr CInt -> CUInt -> IO CSize
sysctlGetSize oid len = sysctlRead oid len nullPtr 0 (const return)

-- | Read a storable value from a sysctl node.
-- This is useful to read binary values such as C structures, otherwise
-- the ad-hoc reading functions should be used instead.
sysctlPeek :: forall k a. (SysctlKey k, Storable a) => k -> IO a
sysctlPeek key =
  withKey key $ \oid len ->
    alloca $ \buf ->
      sysctlRead oid len buf (fromIntegral (sizeOf (undefined::a)))
                 (const . peek)

-- | Read a signed integer from a sysctl (the C int type).
sysctlReadInt :: SysctlKey k => k -> IO #{type int}
sysctlReadInt = sysctlPeek

-- | Read an unsigned integer from a sysctl (the C unsigned int type).
sysctlReadUInt :: SysctlKey k => k -> IO #{type unsigned int}
sysctlReadUInt = sysctlPeek

-- | Read a signed long integer from a sysctl (the C long type).
sysctlReadLong :: SysctlKey k => k -> IO #{type long}
sysctlReadLong = sysctlPeek

-- | Read an unsigned long integer from a sysctl (the C unsigned long type).
sysctlReadULong :: SysctlKey k => k -> IO #{type unsigned long}
sysctlReadULong = sysctlPeek

-- | Read a signed 64-bit integer from a sysctl.
sysctlReadQuad :: SysctlKey k => k -> IO Int64
sysctlReadQuad = sysctlPeek

-- | Read an unsigned 64-bit integer from a sysctl.
sysctlReadUQuad :: SysctlKey k => k -> IO Word64
sysctlReadUQuad = sysctlPeek

-- Useful specialisation of sysctlRead for when the size of the data isn't
-- statically known, and also potentially variable with time.
sysctlReadDynamic :: SysctlKey k => k -> (CSize -> CSize) -> (Ptr a -> CSize -> IO b) -> IO b
sysctlReadDynamic key scale f =
  withKey key $ \oid len -> do
    size <- sysctlGetSize oid len
    let bufSize = scale size  -- Allows to make room for lists of variable length
    allocaBytes (fromIntegral bufSize) $ \buf ->
      sysctlRead oid len buf bufSize f

-- | Like 'sysctlPeek', but allows to retrieve a list of elements whose
-- length can possibly change at runtime.
sysctlPeekArray :: forall k a. (SysctlKey k, Storable a) => k -> IO [a]
sysctlPeekArray key =
  sysctlReadDynamic key (*2) $ \buf size ->
    peekArray (fromIntegral size `div` sizeOf (undefined::a)) buf

-- | Read a string from a sysctl.  If the string can possibly change with
-- time, use 'sysctlPeekArray' for characters instead.
sysctlReadString :: SysctlKey k => k -> IO String
sysctlReadString key =
  sysctlReadDynamic key id (curry (peekCStringLen . second ((subtract 1) . fromIntegral)))

-- Base primitive for all writing operations.
sysctlWrite :: Ptr CInt -> CUInt -> Ptr a -> CSize -> IO ()
sysctlWrite oid len buf size =
  throwErrnoIfMinus1_ "sysctl" (c_sysctl oid len nullPtr nullPtr buf size)

-- | Write a storable value to a sysctl node.
-- This is useful to write binary values such as C structures, otherwise
-- the ad-hoc writing functions should be used instead.
sysctlPoke :: (SysctlKey k, Storable a) => k -> a -> IO ()
sysctlPoke key x =
  withKey key $ \oid len ->
    with x $ \buf -> sysctlWrite oid len buf (fromIntegral (sizeOf buf))

-- | Write a signed integer to a sysctl (the C int type).
sysctlWriteInt :: SysctlKey k => k -> #{type int} -> IO ()
sysctlWriteInt = sysctlPoke

-- | Write an unsigned integer to a sysctl (the C unsigned int type).
sysctlWriteUInt :: SysctlKey k => k -> #{type unsigned int} -> IO ()
sysctlWriteUInt = sysctlPoke

-- | Write a signed long integer to a sysctl (the C long type).
sysctlWriteLong :: SysctlKey k => k -> #{type long} -> IO ()
sysctlWriteLong = sysctlPoke

-- | Write an unsigned long integer to a sysctl (the C unsigned long type).
sysctlWriteULong :: SysctlKey k => k -> #{type unsigned long} -> IO ()
sysctlWriteULong = sysctlPoke

-- | Write a signed 64-bit integer to a sysctl.
sysctlWriteQuad :: SysctlKey k => k -> Int64 -> IO ()
sysctlWriteQuad = sysctlPoke

-- | Write an unsigned 64-bit integer to a sysctl.
sysctlWriteUQuad :: SysctlKey k => k -> Word64 -> IO ()
sysctlWriteUQuad = sysctlPoke

-- | Write a string to a sysctl.
sysctlWriteString :: SysctlKey k => k -> String -> IO ()
sysctlWriteString key s =
  withKey key $ \oid len ->
    withCStringLen s $ \(cs,slen) -> sysctlWrite oid len cs (fromIntegral slen)
