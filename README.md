The `bsd-sysctl` package [![Build Status](https://travis-ci.org/p-alik/bsd-sysctl.png?branch=master)](https://travis-ci.org/p-alik/bsd-sysctl)
===========

This module provides efficient access to the BSD sysctl(3) interface via the
Haskell FFI.
.
It allows to read and write both basic sysctl types, as well as complex opaque
types (typically C structures) described via Storable instances.

See [`bsd-sysctl` on Hackage](https://hackage.haskell.org/package/bsd-sysctl) for more information.

[Demo.hsc](demo/Demo.hsc) contains a sample implementation.
