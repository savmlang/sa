# SaVM Toolkit

This repository is the SaVM toolkit consisting of

- Packer (sapacker)
- Assembler (sasm)
- Runner (savmrun, savmimgrun, savmrt)
- File Testing + Benchmark Suite (satest)
- JIT Memory Manager (sajit)
- Runtime Layer (sart)
- VM Core Library (savm)
- VM Allocator (salloc)
- FFI Layer (saffi)

## MSRV

The SaVM Toolkit is guaranteed to compile on the latest stable
version of `rustc`.

We recommend atleast :

```sh
rustc 1.95.0 (59807616e 2026-04-14)
```

Some platforms require `-Zbuild-std`, which currently
requires nightly Rust.

and for consistency, our build system also uses nightly Rust.
