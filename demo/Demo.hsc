#include <sys/types.h>
#include <sys/user.h>
#include <sys/time.h>
#include <sys/sysctl.h>
-- vim:filetype=haskell
module Main where

import System.BSD.Sysctl
import System.Posix.Types
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

-- This demo program is designed to run on FreeBSD; other BSD systems
-- are likely to have different names and/or types for these sysctls.

data TimeVal = TimeVal CTime CLong

instance Storable TimeVal where
  sizeOf _    = #{size struct timeval}
  alignment _ = alignment (undefined::CTime)
  peek ptr    = do sec  <- #{peek struct timeval, tv_sec} ptr
                   usec <- #{peek struct timeval, tv_usec} ptr
                   return (TimeVal sec usec)

instance Show TimeVal where
  showsPrec p (TimeVal sec usec) = showString "{ usec = " .
                                   showsPrec p sec .
                                   showString ", usec = " .
                                   showsPrec p usec .
                                   showString " }"

data Proc = Proc CPid CUid String

instance Storable Proc where
  sizeOf _    = #{size struct kinfo_proc}
  alignment _ = alignment (undefined::CInt)
  peek ptr    = do pid <- #{peek struct kinfo_proc, ki_pid} ptr
                   uid <- #{peek struct kinfo_proc, ki_uid} ptr
                   cmd <- peekCString (#{ptr struct kinfo_proc, ki_comm} ptr)
                   return (Proc pid uid cmd )

instance Show Proc where
  showsPrec p (Proc pid uid cmd) = showsPrec p pid .
                                   showString "\t" .
                                   showsPrec p uid .
                                   showString ('\t':cmd)

main :: IO ()
main = do osrelease <- sysctlReadString "kern.osrelease"
          putStrLn ("kern.osrelease: " ++ osrelease)
          tv <- sysctlPeek "kern.boottime" :: IO TimeVal
          putStrLn ("kern.boottime: " ++ show tv)
          maxfiles <- sysctlNameToOid "kern.maxfiles" >>= sysctlReadInt
          putStrLn ("kern.maxfiles: " ++ show maxfiles)
          lastpid <- sysctlReadInt "kern.lastpid"
          putStrLn ("kern.lastpid: " ++ show lastpid)
          numvnodes <- sysctlReadLong "vfs.numvnodes"
          putStrLn ("vfs.numvnodes: " ++ show numvnodes)
          recvspace <- sysctlReadULong "net.inet.tcp.recvspace"
          putStrLn ("net.inet.tcp.recvspace: " ++ show recvspace)
          procs <- sysctlPeekArray "kern.proc.all" :: IO [Proc]
          putStrLn "PID\tUID\tCOMMAND"
          mapM_ print procs
          sysctlWriteInt "vfs.usermount" 0 -- Will explode if not root
