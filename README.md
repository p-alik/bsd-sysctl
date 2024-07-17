The `bsd-sysctl` package ![Build Status](https://github.com/p-alik/bsd-sysctl/actions/workflows/haskell.yml/badge.svg?branch=master)
===========

This module provides efficient access to the BSD [`sysctl(3)`](https://man.freebsd.org/cgi/man.cgi?sysctl(3)) interface via the
Haskell FFI.

It allows to read and write both basic sysctl types, as well as complex opaque
types (typically C structures) described via Storable instances.

See [`bsd-sysctl` on Hackage](https://hackage.haskell.org/package/bsd-sysctl) for more information.

[Demo.hsc](demo/Demo.hsc) contains a sample implementation.
