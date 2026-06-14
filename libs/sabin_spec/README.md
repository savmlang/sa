# SaBIN Specification

This project parses the SaBIN file based on the specification.

## What is a SaBIN file?

SaBIN stands for **Sa Binary**. It is the standard file format
used for applications distributed via SaVM.

# Variants

For better handling, SaBIN file formats are of 3 types.

## SaBIN File (.sabin)

This binary is assumed to be running as a userspace ONLY program
the current working directory is assumed to be the cache space.

Binaries in this format are accompanied by **pre-extracted** directories
for the respective system libraries.

This is the standard file format to be used in projects

## SaBIN Image File (.sabinimg)

This is just like the **SaBIN** file but it also has a **SaTable** for the
native libraries using **SaTripleSlim** format

## SaBIN RT File (.sartimg)

This is also an image file but it is supposed to be installed in userspace
"System Program Files" or "Current User Program File", like `.sabin` it also
stored ONLY the bytecode but the SaRT (Sa Runtime) runner is used to execute
this application. This is synonymous to a Managed Runtime handling the application

Also, priviledged services are used to manage a global cache for these applications.

# Anatomy

All the three **SaBIN File Variants** are serialized and deserialized in EXACTLY the
same methodology with no differences. Only the header values and file extension can
differentiate them.

Everything uses **LE == Little-Endian**

## Prologue

Every SaBIN file must start with these magic bytes

`83 97 86 77 66 73 78 45 65 84 111 66`

**ASCII:** `SaVMBIN-AToB`

where `AToB` stands for `A Thing of Beauty (is a joy forever)`

## Header

The Header is defined is (ascending order)

### Version

`<version (u16)>`

We hope ~16k versions would be more than enough.

**Current Version:** 0

### VerId

`<flags (u8)>`

Flags is a MIXEDRADIX of following:

- Padding
- Compact (2 states): 0: AppId=u64, 1: AppId=u32
- SaBIN Format (3 states) : 0: sabin, 1: sabinimg, 2: sartimg

### Metadata Keys

`<TOTAL KEYS (u16)> ([size (u8)][value (Size bytes)])*`

### Global Meta

`<Total Libraries (AppId)> <Total CallDecls (AppId)> <Last Section Index (AppId)>`

### RO Data

`[Size (u32)] [Ptr (u40)]`

### RW Data

`[Size (u32)] [Ptr (u40)]`

### PGO

`[Size (u8)] (Bytecode Section ID (AppId))* [Size (u8)] (Bytecode Section ID (AppId))*`

The first queue is the **Instantaneous Priority Queue**. These are directly optimized to the higher tier
of Optimizations available in the sys.

The second queue is the **Priority Queue**. These are optimized using a separete priority queue worker
different from the public queue worker.

**Compact Mode** increases the RAM footprint by ~4080Bytes maxima (considering Size = 255)

### BytecodeMap

`([Size (u24)][Ptr (u40)] (u64))*`

- Ptr : It is the absolute address of the 1st byte of the data from the root of the file being `0`
- Size : This is a 24bit for total BYTES (~16MiB)

If size==0, it is classified as a immediate loadable libcall. The pointer points to a **libcall definition**.

### CallDeclTable

`([Size (u24)][Ptr (u40)] (u64))*`

- Ptr : It is the absolute address of the 1st byte of the data from the root of the file being `0`
- Size : This is a 24bit for total BYTES (~16MiB)

This is parsed as the CallDecl as defined in `sart`

### SaTable

`( SaTripleSlim (8bytes), Size (u32), Ptr (u40) )*`

This SaTable must be present on all sabin files but the `Ptr` is allowed to be NULL
to save space.

Conversely, for `sabinimg`, NULL `Ptr` indicates absence of platform specific dependency.

The Ptr contains a fully ZIP-ed archive to be extracted fully.

### Libcall Definition

`[Library ID (64bits)]`

---

**Note:** Because the Ptr points to a ZIP archive, SaBIN Compression becomes entirely plausible
and binary authors can even decide their levels according to their needs.
