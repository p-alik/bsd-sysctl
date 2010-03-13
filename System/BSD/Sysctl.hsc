{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- vim:filetype=haskell
-----------------------------------------------------------------------------
-- |
-- Module      :  System.BSD.Sysctl
-- Copyright   :  (c) Maxime Henrion 2009
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
-- Storable instance.  It is also possible to retrieve data whose size changes
-- at runtime with the 'sysctlPeekArray' function.
--
-- Nodes may be queried either by their name, their OID as a list of 'Int's, or
-- by an OID returned by 'sysctlNameToOid' if speed is a concern.
-------------------------------------------------------------------------------

#include <sys/param.h>
#include <sys/sysctl.h>
module System.BSD.Sysctl (
  -- * The data types
  SysctlKey,		-- The class of types that can be used to identify a node
  OID,			-- The @OID@ datatype identifies a sysctl node

  -- * Name to OID conversion
  sysctlNameToOid,	-- :: String -> IO OID

  -- * Basic reading functions
  sysctlReadInt,	-- :: SysctlKey k => k -> IO #{type int}
  sysctlReadUInt,	-- :: SysctlKey k => k -> IO #{type unsigned int}
  sysctlReadLong,	-- :: SysctlKey k => k -> IO #{type long}
  sysctlReadULong,	-- :: SysctlKey k => k -> IO #{type unsigned long}
  sysctlReadQuad,	-- :: SysctlKey k => k -> IO Int64
  sysctlReadUQuad,	-- :: SysctlKey k => k -> IO Word64
  sysctlReadString,	-- :: SysctlKey k => k -> IO String

  -- * Advanced reading functions
  sysctlPeek,		-- :: forall k a. (SysctlKey k, Storable a) => k -> IO a
  sysctlPeekArray,	-- :: forall k a. (SysctlKey k, Storable a) => k -> IO [a]

  -- * Basic writing functions
  sysctlWriteInt,	-- :: SysctlKey k => k -> #{type int} -> IO ()
  sysctlWriteUInt,	-- :: SysctlKey k => k -> #{type unsigned int} -> IO ()
  sysctlWriteLong,	-- :: SysctlKey k => k -> #{type long} -> IO ()
  sysctlWriteULong,	-- :: SysctlKey k => k -> #{type unsigned long} -> IO ()
  sysctlWriteQuad,	-- :: SysctlKey k => k -> Int64 -> IO ()
  sysctlWriteUQuad,	-- :: SysctlKey k => k -> Word64 -> IO ()
  sysctlWriteString,	-- :: SysctlKey k => k -> String -> IO ()

  -- * Advanced writing functions
  sysctlPoke		-- :: (SysctlKey k, Storable a) => k -> a -> IO ()
  ) where

import Control.Arrow (second)
import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import Foreign.ForeignPtr

data OID = OID {-# UNPACK #-} !(ForeignPtr CInt)
               {-# UNPACK #-} !CUInt

class SysctlKey k where
  withKey :: k -> (Ptr CInt -> CUInt -> IO a) -> IO a

instance SysctlKey OID where
  withKey (OID fp len) f = withForeignPtr fp (\ptr -> f ptr len)

instance SysctlKey String where
  withKey name f = sysctlNameToOid name >>= flip withKey f

instance SysctlKey [Int] where
  withKey oid f = withArrayLen (map fromIntegral oid)
                               (\len ptr -> f ptr (fromIntegral len))

foreign import ccall unsafe "sysctl"
  c_sysctl :: Ptr CInt -> CUInt -> Ptr a -> Ptr CSize -> Ptr b -> CSize -> IO CInt

foreign import ccall unsafe "sysctlnametomib"
  c_sysctlnametomib :: CString -> Ptr CInt -> Ptr CSize -> IO CInt

-- Call sysctl with a size set to 0 to retrieve the size of the object.
sysctlGetSize :: Ptr CInt -> CUInt -> IO CSize
sysctlGetSize oid len = sysctlRead oid len nullPtr 0 (const return)

-- Get the OID corresponding to a sysctl name.
sysctlNameToOid :: String -> IO OID
sysctlNameToOid name =
  withCString name $ \cname -> do
    fp  <- mallocForeignPtrArray (fromIntegral maxlen)
    len <- withForeignPtr fp $ \oid ->
             alloca $ \sizePtr -> do
               poke sizePtr maxlen
               throwErrnoIfMinus1_ "sysctlnametomib"
                 (c_sysctlnametomib cname oid sizePtr)
               peek sizePtr
    return (OID fp (fromIntegral len))
  where maxlen = #{const CTL_MAXNAME}

{-
-- This could be used to implement some form of type checking at runtime some
-- day, but the interface is undocumented and probably unportable though.
oidToType :: Ptr CInt -> CUInt -> IO (CUInt, String)
oidToType oid len =
  let len' = len + 2 in
    allocaArray (fromIntegral len') $ \oid' ->
      allocaBytes defaultBufSize $ \buf ->
        alloca $ \sizePtr ->
          do poke oid' 0
             poke (oid' `advancePtr` 1) 4
             copyArray (oid' `advancePtr` 2) oid (fromIntegral len)
             poke sizePtr (fromIntegral defaultBufSize)
             throwErrnoIfMinus1_ "sysctl"
               (c_sysctl oid' len' buf sizePtr nullPtr 0)
             kind <- peek buf
             fmt  <- peekCString (buf `plusPtr` (sizeOf kind))
             return (kind, fmt)
  where defaultBufSize = 1024 -- as in FreeBSD's libc
-}

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

-- Read a sysctl value that is an instance of Storable.
sysctlPeek :: forall k a. (SysctlKey k, Storable a) => k -> IO a
sysctlPeek key =
  withKey key $ \oid len ->
    alloca $ \buf ->
      sysctlRead oid len buf (fromIntegral (sizeOf (undefined::a)))
                 (const . peek)

sysctlReadInt :: SysctlKey k => k -> IO #{type int}
sysctlReadInt = sysctlPeek

sysctlReadUInt :: SysctlKey k => k -> IO #{type unsigned int}
sysctlReadUInt = sysctlPeek

sysctlReadLong :: SysctlKey k => k -> IO #{type long}
sysctlReadLong = sysctlPeek

sysctlReadULong :: SysctlKey k => k -> IO #{type unsigned long}
sysctlReadULong = sysctlPeek

sysctlReadQuad :: SysctlKey k => k -> IO Int64
sysctlReadQuad = sysctlPeek

sysctlReadUQuad :: SysctlKey k => k -> IO Word64
sysctlReadUQuad = sysctlPeek

-- Useful specialisation of sysctlRead for when the size of the data isn't
-- statically known, and also potentially variable with time.
sysctlReadDynamic :: SysctlKey k => k -> (CSize -> CSize) -> (Ptr a -> CSize -> IO b) -> IO b
sysctlReadDynamic key scale f =
  withKey key $ \oid len -> do
    size <- sysctlGetSize oid len
    let bufSize = scale size	-- Allows to make room for lists of variable length
    allocaBytes (fromIntegral bufSize) $ \buf ->
      sysctlRead oid len buf bufSize f

-- Retrieve a variable number of elements from a sysctl.
sysctlPeekArray :: forall k a. (SysctlKey k, Storable a) => k -> IO [a]
sysctlPeekArray key =
  sysctlReadDynamic key (*2) $ \buf size ->
    peekArray (fromIntegral size `div` sizeOf (undefined::a)) buf

-- Read a String from a sysctl.  If the string can possibly change with
-- time, use sysctlPeekArray instead.
sysctlReadString :: SysctlKey k => k -> IO String
sysctlReadString key =
  sysctlReadDynamic key id (curry (peekCStringLen . second ((subtract 1) . fromIntegral)))

-- Base primitive for all writing operations.
sysctlWrite :: Ptr CInt -> CUInt -> Ptr a -> CSize -> IO ()
sysctlWrite oid len buf size =
  throwErrnoIfMinus1_ "sysctl" (c_sysctl oid len nullPtr nullPtr buf size)

sysctlPoke :: (SysctlKey k, Storable a) => k -> a -> IO ()
sysctlPoke key x =
  withKey key $ \oid len ->
    with x $ \buf -> sysctlWrite oid len buf (fromIntegral (sizeOf buf))

sysctlWriteInt :: SysctlKey k => k -> #{type int} -> IO ()
sysctlWriteInt = sysctlPoke

sysctlWriteUInt :: SysctlKey k => k -> #{type unsigned int} -> IO ()
sysctlWriteUInt = sysctlPoke

sysctlWriteLong :: SysctlKey k => k -> #{type long} -> IO ()
sysctlWriteLong = sysctlPoke

sysctlWriteULong :: SysctlKey k => k -> #{type unsigned long} -> IO ()
sysctlWriteULong = sysctlPoke

sysctlWriteQuad :: SysctlKey k => k -> Int64 -> IO ()
sysctlWriteQuad = sysctlPoke

sysctlWriteUQuad :: SysctlKey k => k -> Word64 -> IO ()
sysctlWriteUQuad = sysctlPoke

sysctlWriteString :: SysctlKey k => k -> String -> IO ()
sysctlWriteString key s =
  withKey key $ \oid len ->
    withCStringLen s $ \(cs,slen) -> sysctlWrite oid len cs (fromIntegral slen)
