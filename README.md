# SaVM Toolkit

This repository is the savm toolkit consisting of

- Packer (sapacker)
- Assembler (sasm)
- Runner (savmrun)
- Filetester + Benchmarker (satest)
- JIT Memory Manager (sajit)
- Runtime Layer (sart)
- VM Core Library (savm)
- VM Allocator (salloc)
- FFI Layer (saffi)

## MSRV

The SaVM Toolkit is only guaranteed to compile on the latest and greatest
version of the `rustc +nightly` compiler.

We recommend atleast the below:

```sh
rustc 1.97.0-nightly (ff9a9ea07 2026-05-13)
```
