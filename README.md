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

> **TL;DR** Last 3 stable versions

The Toolkit prioritizes the rapid adoption of stabilized Rust features. However, for sanity concerns, efforts are made to support the **last 3 stable versions**.

The toolkit must compile on at least:

### Stable

```sh
rustc 1.95.0
```

### Nightly

_(used by the build environment as on 19-July-2026::7:56:00IST)_

```sh
rustc 1.99.0-nightly (eff8269f7 2026-07-18)
```

Tier-3 rust platforms require `-Zbuild-std`, which currently
requires nightly Rust.

and for consistency, our build system also uses nightly Rust.
