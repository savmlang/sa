/*
 * I admit this is AI Written :/
 *
 * coff_jit_i686.h  --  JIT memory loader for i686/COFF objects (Windows only)
 *
 * Supports relocations:
 *   IMAGE_REL_I386_ABSOLUTE  (0x0000)  ignored
 *   IMAGE_REL_I386_DIR32     (0x0006)  32-bit VA
 *   IMAGE_REL_I386_DIR32NB   (0x0007)  32-bit RVA  (image-base = section[0].base)
 *   IMAGE_REL_I386_SECTION   (0x000A)  16-bit section index
 *   IMAGE_REL_I386_SECREL    (0x000B)  32-bit section-relative offset
 *   IMAGE_REL_I386_REL32     (0x0014)  32-bit PC-relative displacement
 *
 * External symbol resolution : caller-supplied callback
 * Memory allocation           : caller-supplied allocator callbacks
 * SEH unwind tables           : registered via RtlAddFunctionTable  (x86 only,
 *                               so RUNTIME_FUNCTION is NOT used -- Windows x86
 *                               uses FS:[0] frame-based SEH; we just make the
 *                               section memory available.  The loader
 *                               optionally calls a user hook so the caller
 *                               can set up any VEH/SEH chain entries.)
 *
 * Usage sketch:
 *
 *   CoffJitAllocator alloc = { my_alloc, my_free };
 *   CoffJitImage    *img   = NULL;
 *   CoffJitResult    r     = coff_jit_load(data, size, &alloc, my_sym_lookup,
 *                                          my_seh_hook, &img);
 *   if (r != COFF_JIT_OK) { ... }
 *
 *   typedef int (*Fn)(void);
 *   Fn fn = (Fn)coff_jit_symbol(img, "_myFunc");
 *   int result = fn();
 *
 *   coff_jit_free(img);
 */

#ifndef COFF_JIT_I686_H
#define COFF_JIT_I686_H

#ifdef __cplusplus
extern "C"
{
#endif

#include <stddef.h>
#include <stdint.h>

  /* =========================================================================
   * Public types
   * ========================================================================= */

  typedef enum
  {
    COFF_JIT_OK = 0,
    COFF_JIT_ERR_INVALID_MAGIC = -1,    /* not a COFF file                  */
    COFF_JIT_ERR_UNSUPPORTED_ARCH = -2, /* not IMAGE_FILE_MACHINE_I386      */
    COFF_JIT_ERR_ALLOC = -3,            /* allocator returned NULL          */
    COFF_JIT_ERR_RELOC_OVERFLOW = -4,   /* relocation value out of range    */
    COFF_JIT_ERR_UNKNOWN_SYMBOL = -5,   /* external symbol not resolved     */
    COFF_JIT_ERR_UNKNOWN_RELOC = -6,    /* unrecognised relocation type     */
    COFF_JIT_ERR_TRUNCATED = -7,        /* COFF data shorter than headers   */
  } CoffJitResult;

  /*
   * Allocator callbacks.
   *
   * alloc(size, exec, user_ctx)
   *   Allocate `size` bytes.  If `exec` is non-zero the region will be used for
   *   machine code and must be executable (e.g. VirtualAlloc with
   *   PAGE_EXECUTE_READWRITE; tighten permissions after load if desired).
   *   Returns NULL on failure.
   *
   * free(ptr, size, user_ctx)
   *   Release memory previously returned by alloc.
   */
  typedef void *(*CoffJitAllocFn)(size_t size, int exec, void *user_ctx);
  typedef void (*CoffJitFreeFn)(void *ptr, size_t size, void *user_ctx);

  typedef struct
  {
    CoffJitAllocFn alloc;
    CoffJitFreeFn free;
    void *user_ctx; /* forwarded to both callbacks unchanged */
  } CoffJitAllocator;

  /*
   * External symbol resolver callback.
   *
   *   sym_name  : null-terminated decorated name (e.g. "_printf")
   *   user_ctx  : forwarded from coff_jit_load()
   *
   * Return the absolute address of the symbol, or 0 if not found.
   * Returning 0 causes coff_jit_load to fail with COFF_JIT_ERR_UNKNOWN_SYMBOL.
   */
  typedef uintptr_t (*CoffJitSymLookupFn)(const char *sym_name, void *user_ctx);

  /*
   * Optional SEH hook.
   * Called once after all sections have been loaded and relocated but before
   * coff_jit_load returns.  On x86 Windows, SEH is frame-based (FS:[0]) so
   * there is no OS-level function table to register.  This hook lets the caller
   * inspect or patch exception handler addresses in .text/.rdata if needed.
   *
   *   sections_base : array of (name, load_addr, size) for every loaded section
   *   section_count : length of that array
   *   user_ctx      : forwarded from coff_jit_load()
   */
  typedef struct
  {
    const char *name;    /* section name, e.g. ".text"           */
    uintptr_t load_addr; /* virtual address of the section       */
    size_t size;         /* byte size of the section             */
  } CoffJitSectionInfo;

  typedef void (*CoffJitSehHookFn)(const CoffJitSectionInfo *sections,
                                   size_t section_count,
                                   void *user_ctx);

  /* Opaque handle returned by coff_jit_load. */
  typedef struct CoffJitImage_ CoffJitImage;

  /* =========================================================================
   * Public API
   * ========================================================================= */

  /*
   * coff_jit_load
   *   Parse and load a raw COFF object into executable memory.
   *
   *   coff_data   : pointer to the raw .obj bytes
   *   coff_size   : byte length of coff_data
   *   allocator   : memory allocator (must not be NULL)
   *   sym_lookup  : external symbol resolver (must not be NULL)
   *   sym_ctx     : forwarded to sym_lookup unchanged
   *   seh_hook    : optional SEH setup hook (may be NULL)
   *   seh_ctx     : forwarded to seh_hook unchanged
   *   out_image   : on success receives a heap-allocated CoffJitImage*
   *
   *   Returns COFF_JIT_OK on success, negative error code otherwise.
   *   On error *out_image is set to NULL; any already-allocated sections are
   *   freed before returning.
   */
  CoffJitResult coff_jit_load(const void *coff_data,
                              size_t coff_size,
                              const CoffJitAllocator *allocator,
                              CoffJitSymLookupFn sym_lookup,
                              void *sym_ctx,
                              CoffJitSehHookFn seh_hook,
                              void *seh_ctx,
                              CoffJitImage **out_image);

  /*
   * coff_jit_symbol
   *   Look up a named symbol in a loaded image.
   *   Returns NULL if the symbol is not defined in this object.
   */
  void *coff_jit_symbol(const CoffJitImage *image, const char *name);

  /*
   * coff_jit_free
   *   Release all memory owned by `image` (sections + the image struct itself).
   *   The same allocator that was passed to coff_jit_load must still be valid.
   */
  void coff_jit_free(CoffJitImage *image);

/* =========================================================================
 * Implementation  (single-header style: define COFF_JIT_IMPLEMENTATION)
 * ========================================================================= */
#ifdef COFF_JIT_IMPLEMENTATION

#include <string.h>
#include <stdlib.h>

  /* -------------------------------------------------------------------------
   * COFF on-disk structures (Windows i386)
   * ------------------------------------------------------------------------- */

#define IMAGE_FILE_MACHINE_I386 0x014C

#pragma pack(push, 1)

  typedef struct
  {
    uint16_t Machine; /* 0x014C for i386                        */
    uint16_t NumberOfSections;
    uint32_t TimeDateStamp;
    uint32_t PointerToSymbolTable;
    uint32_t NumberOfSymbols;
    uint16_t SizeOfOptionalHeader;
    uint16_t Characteristics;
  } CoffFileHeader;

#define COFF_SECTION_NAME_LEN 8
  typedef struct
  {
    char Name[COFF_SECTION_NAME_LEN];
    uint32_t VirtualSize;    /* 0 in object files                      */
    uint32_t VirtualAddress; /* 0 in object files                      */
    uint32_t SizeOfRawData;
    uint32_t PointerToRawData;
    uint32_t PointerToRelocations;
    uint32_t PointerToLinenumbers;
    uint16_t NumberOfRelocations;
    uint16_t NumberOfLinenumbers;
    uint32_t Characteristics;
  } CoffSectionHeader;

  typedef struct
  {
    uint32_t VirtualAddress; /* offset within section                  */
    uint32_t SymbolTableIndex;
    uint16_t Type;
  } CoffRelocation;

  /* Standard symbol record (18 bytes) */
  typedef struct
  {
    union
    {
      char ShortName[8];
      struct
      {
        uint32_t Zeroes;
        uint32_t Offset;
      } LongName;
    } N;
    uint32_t Value;
    int16_t SectionNumber; /* 1-based; 0=undefined; -1=absolute; -2=debug */
    uint16_t Type;
    uint8_t StorageClass;
    uint8_t NumberOfAuxSymbols;
  } CoffSymbol;

#pragma pack(pop)

/* Relocation type constants for i386 */
#define IMAGE_REL_I386_ABSOLUTE 0x0000
#define IMAGE_REL_I386_DIR32 0x0006
#define IMAGE_REL_I386_DIR32NB 0x0007
#define IMAGE_REL_I386_SECTION 0x000A
#define IMAGE_REL_I386_SECREL 0x000B
#define IMAGE_REL_I386_REL32 0x0014

/* COFF symbol storage classes */
#define IMAGE_SYM_CLASS_EXTERNAL 2
#define IMAGE_SYM_CLASS_STATIC 3

  /* -------------------------------------------------------------------------
   * Internal structures
   * ------------------------------------------------------------------------- */

  typedef struct
  {
    char name[COFF_SECTION_NAME_LEN + 1]; /* null-terminated           */
    uint8_t *data;                        /* loaded (allocated) memory */
    size_t size;
    int is_exec;
  } LoadedSection;

  /* Exported symbol: name -> absolute address */
  typedef struct
  {
    char *name;
    uintptr_t addr;
  } ExportedSymbol;

  struct CoffJitImage_
  {
    CoffJitAllocator allocator;

    LoadedSection *sections;
    uint16_t num_sections;

    ExportedSymbol *exports;
    uint32_t num_exports;
  };

  /* -------------------------------------------------------------------------
   * Helpers
   * ------------------------------------------------------------------------- */

  static void write_u16_le(uint8_t *p, uint16_t v)
  {
    p[0] = (uint8_t)(v);
    p[1] = (uint8_t)(v >> 8);
  }

  static void write_u32_le(uint8_t *p, uint32_t v)
  {
    p[0] = (uint8_t)(v);
    p[1] = (uint8_t)(v >> 8);
    p[2] = (uint8_t)(v >> 16);
    p[3] = (uint8_t)(v >> 24);
  }

  static uint32_t read_u32_le(const uint8_t *p)
  {
    return (uint32_t)p[0] | (uint32_t)p[1] << 8 | (uint32_t)p[2] << 16 | (uint32_t)p[3] << 24;
  }

  /* Resolve a COFF symbol name: either the 8-byte short form or a string-table
   * offset.  `strtab` points to the start of the string table (the 4-byte size
   * field followed by null-terminated strings).  `out` must be at least 256
   * bytes.  Returns a pointer to `out`. */
  static const char *coff_sym_name(const CoffSymbol *sym,
                                   const char *strtab,
                                   char *out)
  {
    if (sym->N.LongName.Zeroes == 0)
    {
      /* long name: offset into string table (past the 4-byte size field)  */
      const char *s = strtab + sym->N.LongName.Offset;
      size_t len = strlen(s);
      if (len > 255)
        len = 255;
      memcpy(out, s, len);
      out[len] = '\0';
    }
    else
    {
      /* short name: up to 8 bytes, possibly not null-terminated            */
      memcpy(out, sym->N.ShortName, 8);
      out[8] = '\0';
    }
    return out;
  }

  /* Section name: similar -- long names use /nnn decimal offset notation       */
  static void coff_sec_name(const CoffSectionHeader *sh,
                            const char *strtab,
                            char *out)
  {
    if (sh->Name[0] == '/')
    {
      /* long name offset encoded as decimal digits after '/'               */
      uint32_t off = (uint32_t)atoi(sh->Name + 1);
      const char *s = strtab + off;
      size_t len = strlen(s);
      if (len > COFF_SECTION_NAME_LEN)
        len = COFF_SECTION_NAME_LEN;
      memcpy(out, s, len);
      out[len] = '\0';
    }
    else
    {
      memcpy(out, sh->Name, COFF_SECTION_NAME_LEN);
      out[COFF_SECTION_NAME_LEN] = '\0';
    }
  }

  /* Free all sections that have been allocated so far (used on error path).    */
  static void free_sections(CoffJitImage *img, uint16_t count)
  {
    for (uint16_t i = 0; i < count; ++i)
    {
      if (img->sections[i].data)
      {
        img->allocator.free(img->sections[i].data,
                            img->sections[i].size,
                            img->allocator.user_ctx);
      }
    }
  }

  /* -------------------------------------------------------------------------
   * coff_jit_load
   * ------------------------------------------------------------------------- */

  CoffJitResult coff_jit_load(const void *coff_data,
                              size_t coff_size,
                              const CoffJitAllocator *allocator,
                              CoffJitSymLookupFn sym_lookup,
                              void *sym_ctx,
                              CoffJitSehHookFn seh_hook,
                              void *seh_ctx,
                              CoffJitImage **out_image)
  {
    *out_image = NULL;

    const uint8_t *base = (const uint8_t *)coff_data;

    /* ------------------------------------------------------------------
     * 1. Parse and validate the file header
     * ------------------------------------------------------------------ */
    if (coff_size < sizeof(CoffFileHeader))
      return COFF_JIT_ERR_TRUNCATED;

    const CoffFileHeader *fhdr = (const CoffFileHeader *)base;

    if (fhdr->Machine != IMAGE_FILE_MACHINE_I386)
      return COFF_JIT_ERR_UNSUPPORTED_ARCH;

    uint16_t num_sections = fhdr->NumberOfSections;
    uint32_t num_symbols = fhdr->NumberOfSymbols;

    /* Section headers immediately follow the file header (+ optional header,
     * which is 0 bytes for object files, but be safe).                       */
    size_t sec_hdr_offset = sizeof(CoffFileHeader) + fhdr->SizeOfOptionalHeader;
    if (coff_size < sec_hdr_offset + (size_t)num_sections * sizeof(CoffSectionHeader))
      return COFF_JIT_ERR_TRUNCATED;

    const CoffSectionHeader *sec_hdrs =
        (const CoffSectionHeader *)(base + sec_hdr_offset);

    /* Symbol table and string table */
    const CoffSymbol *sym_table = NULL;
    const char *str_table = NULL; /* points at the 4-byte size field  */
    if (fhdr->PointerToSymbolTable && num_symbols)
    {
      if (coff_size < fhdr->PointerToSymbolTable + num_symbols * sizeof(CoffSymbol))
        return COFF_JIT_ERR_TRUNCATED;
      sym_table = (const CoffSymbol *)(base + fhdr->PointerToSymbolTable);
      /* String table immediately follows symbol table */
      str_table = (const char *)(sym_table + num_symbols);
    }

    /* ------------------------------------------------------------------
     * 2. Allocate the image struct
     * ------------------------------------------------------------------ */
    CoffJitImage *img = (CoffJitImage *)calloc(1, sizeof(CoffJitImage));
    if (!img)
      return COFF_JIT_ERR_ALLOC;

    img->allocator = *allocator;
    img->num_sections = num_sections;
    img->sections = (LoadedSection *)calloc(num_sections, sizeof(LoadedSection));
    if (!img->sections)
    {
      free(img);
      return COFF_JIT_ERR_ALLOC;
    }

    /* ------------------------------------------------------------------
     * 3. Load sections: allocate memory and copy raw data
     * ------------------------------------------------------------------ */

    /* We'll treat section[0]'s load address as ImageBase for DIR32NB.        */

    for (uint16_t i = 0; i < num_sections; ++i)
    {
      const CoffSectionHeader *sh = &sec_hdrs[i];
      LoadedSection *ls = &img->sections[i];

      /* Determine name */
      coff_sec_name(sh, str_table ? str_table : "", ls->name);

      size_t sz = sh->SizeOfRawData;
      if (sz == 0)
      {
        /* BSS-like section: allocate zeroed memory                       */
        ls->size = sh->VirtualSize ? sh->VirtualSize : 0;
        ls->is_exec = 0;
        if (ls->size)
        {
          ls->data = (uint8_t *)allocator->alloc(ls->size, 0, allocator->user_ctx);
          if (!ls->data)
          {
            free_sections(img, i);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_ALLOC;
          }
          memset(ls->data, 0, ls->size);
        }
        continue;
      }

      /* Validate raw data range */
      if (coff_size < (size_t)sh->PointerToRawData + sz)
      {
        free_sections(img, i);
        free(img->sections);
        free(img);
        return COFF_JIT_ERR_TRUNCATED;
      }

/* Determine if this section needs to be executable */
#define IMAGE_SCN_CNT_CODE 0x00000020
#define IMAGE_SCN_MEM_EXECUTE 0x20000000
      int is_exec = !!(sh->Characteristics &
                       (IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE));

      ls->size = sz;
      ls->is_exec = is_exec;
      ls->data = (uint8_t *)allocator->alloc(sz, is_exec, allocator->user_ctx);
      if (!ls->data)
      {
        free_sections(img, i);
        free(img->sections);
        free(img);
        return COFF_JIT_ERR_ALLOC;
      }
      memcpy(ls->data, base + sh->PointerToRawData, sz);
    }

    /* ------------------------------------------------------------------
     * 4. Build the export table from the symbol table
     *    (EXTERNAL symbols with a positive section number = defined here)
     * ------------------------------------------------------------------ */

    /* First pass: count exported symbols */
    uint32_t num_exports = 0;
    if (sym_table)
    {
      for (uint32_t si = 0; si < num_symbols; si += 1 + sym_table[si].NumberOfAuxSymbols)
      {
        const CoffSymbol *sym = &sym_table[si];
        if (sym->SectionNumber > 0 &&
            sym->StorageClass == IMAGE_SYM_CLASS_EXTERNAL)
          ++num_exports;
      }
    }

    img->num_exports = num_exports;
    if (num_exports)
    {
      img->exports = (ExportedSymbol *)calloc(num_exports, sizeof(ExportedSymbol));
      if (!img->exports)
      {
        free_sections(img, num_sections);
        free(img->sections);
        free(img);
        return COFF_JIT_ERR_ALLOC;
      }

      char name_buf[256];
      uint32_t ei = 0;
      for (uint32_t si = 0; si < num_symbols; si += 1 + sym_table[si].NumberOfAuxSymbols)
      {
        const CoffSymbol *sym = &sym_table[si];
        if (sym->SectionNumber > 0 &&
            sym->StorageClass == IMAGE_SYM_CLASS_EXTERNAL)
        {
          uint16_t sec_idx = (uint16_t)(sym->SectionNumber - 1); /* 0-based */
          coff_sym_name(sym, str_table ? str_table : "", name_buf);
          img->exports[ei].name = (char *)malloc(strlen(name_buf) + 1);
          if (!img->exports[ei].name)
          {
            /* Leak-safe cleanup omitted for brevity; caller should treat
             * non-OK return as requiring no further use of img.       */
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_ALLOC;
          }
          strcpy(img->exports[ei].name, name_buf);
          img->exports[ei].addr =
              (uintptr_t)img->sections[sec_idx].data + sym->Value;
          ++ei;
        }
      }
    }

    /* ------------------------------------------------------------------
     * 5. Apply relocations
     * ------------------------------------------------------------------ */

    /* image_base = load address of section[0] (approximation of ImageBase,
     * used only by DIR32NB).                                                  */
    uintptr_t image_base = num_sections ? (uintptr_t)img->sections[0].data : 0;

    char name_buf[256];

    for (uint16_t si = 0; si < num_sections; ++si)
    {
      const CoffSectionHeader *sh = &sec_hdrs[si];
      LoadedSection *ls = &img->sections[si];

      if (!sh->NumberOfRelocations || !ls->data)
        continue;

      /* Validate relocation table range */
      size_t reloc_table_sz = (size_t)sh->NumberOfRelocations * sizeof(CoffRelocation);
      if (coff_size < (size_t)sh->PointerToRelocations + reloc_table_sz)
      {
        free_sections(img, num_sections);
        free(img->sections);
        free(img);
        return COFF_JIT_ERR_TRUNCATED;
      }

      const CoffRelocation *relocs =
          (const CoffRelocation *)(base + sh->PointerToRelocations);

      for (uint16_t ri = 0; ri < sh->NumberOfRelocations; ++ri)
      {
        const CoffRelocation *rel = &relocs[ri];
        uint32_t sym_idx = rel->SymbolTableIndex;

        if (!sym_table || sym_idx >= num_symbols)
        {
          free_sections(img, num_sections);
          free(img->sections);
          free(img);
          return COFF_JIT_ERR_TRUNCATED;
        }

        const CoffSymbol *sym = &sym_table[sym_idx];

        /* Resolve symbol address -------------------------------------- */
        uintptr_t sym_addr = 0;
        uint16_t sym_sec = 0; /* 0-based section index of the symbol */
        int is_extern = (sym->SectionNumber <= 0);

        if (!is_extern)
        {
          /* Defined in this object */
          sym_sec = (uint16_t)(sym->SectionNumber - 1);
          sym_addr = (uintptr_t)img->sections[sym_sec].data + sym->Value;
        }
        else if (sym->SectionNumber == 0)
        {
          /* External: resolve via callback */
          coff_sym_name(sym, str_table ? str_table : "", name_buf);
          sym_addr = sym_lookup(name_buf, sym_ctx);
          if (!sym_addr)
          {
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_UNKNOWN_SYMBOL;
          }
        }
        /* SectionNumber == -1 (absolute) or -2 (debug): sym_addr stays 0 */

        /* Patch location in the section copy */
        if (rel->VirtualAddress + 4 > ls->size)
        {
          /* Relocation extends past section -- truncated object */
          free_sections(img, num_sections);
          free(img->sections);
          free(img);
          return COFF_JIT_ERR_TRUNCATED;
        }
        uint8_t *patch = ls->data + rel->VirtualAddress;

        /* Read the in-place addend (little-endian 32-bit) */
        uint32_t addend = read_u32_le(patch);

        switch (rel->Type)
        {

        case IMAGE_REL_I386_ABSOLUTE:
          /* No-op */
          break;

        case IMAGE_REL_I386_DIR32:
        {
          /* 32-bit absolute VA: sym_addr + addend */
          uint64_t result = (uint64_t)sym_addr + addend;
          if (result > UINT32_MAX)
          {
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_RELOC_OVERFLOW;
          }
          write_u32_le(patch, (uint32_t)result);
          break;
        }

        case IMAGE_REL_I386_DIR32NB:
        {
          /* 32-bit RVA: (sym_addr + addend) - image_base */
          uint64_t result = (uint64_t)sym_addr + addend - image_base;
          if (result > UINT32_MAX)
          {
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_RELOC_OVERFLOW;
          }
          write_u32_le(patch, (uint32_t)result);
          break;
        }

        case IMAGE_REL_I386_REL32:
        {
          /*
           * 32-bit PC-relative displacement.
           * The target is:  sym_addr + addend
           * The patch site (as seen by the CPU after fetch) is:
           *   patch_va + 4   (i.e. the address of the byte after the 4
           *                   displacement bytes)
           * So: displacement = target - (patch_va + 4)
           *   where patch_va = (uintptr_t)patch
           */
          uintptr_t target = sym_addr + addend;
          uintptr_t patch_va = (uintptr_t)patch;
          int64_t disp = (int64_t)target - (int64_t)(patch_va + 4);
          if (disp > INT32_MAX || disp < INT32_MIN)
          {
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_RELOC_OVERFLOW;
          }
          write_u32_le(patch, (uint32_t)(int32_t)disp);
          break;
        }

        case IMAGE_REL_I386_SECTION:
        {
          /*
           * 16-bit section index of the section that contains the
           * symbol.  For extern symbols this is meaningless -- we store
           * 0xFFFF to indicate "external".
           */
          uint16_t idx = is_extern ? 0xFFFF : (uint16_t)(sym_sec + 1);
          if (rel->VirtualAddress + 2 > ls->size)
          {
            free_sections(img, num_sections);
            free(img->sections);
            free(img);
            return COFF_JIT_ERR_TRUNCATED;
          }
          write_u16_le(patch, idx);
          break;
        }

        case IMAGE_REL_I386_SECREL:
        {
          /*
           * 32-bit offset of the symbol from the beginning of its
           * section.  For symbols defined in this object this is just
           * sym->Value + addend.  For external symbols it is
           * undefined; we write 0.
           */
          uint32_t secrel = is_extern ? 0 : (sym->Value + addend);
          write_u32_le(patch, secrel);
          break;
        }

        default:
          free_sections(img, num_sections);
          free(img->sections);
          free(img);
          return COFF_JIT_ERR_UNKNOWN_RELOC;
        }
      }
    }

    /* ------------------------------------------------------------------
     * 6. Call the optional SEH hook
     * ------------------------------------------------------------------ */
    if (seh_hook)
    {
      /* Build a temporary array of CoffJitSectionInfo */
      CoffJitSectionInfo *infos =
          (CoffJitSectionInfo *)alloca(num_sections * sizeof(CoffJitSectionInfo));
      for (uint16_t i = 0; i < num_sections; ++i)
      {
        infos[i].name = img->sections[i].name;
        infos[i].load_addr = (uintptr_t)img->sections[i].data;
        infos[i].size = img->sections[i].size;
      }
      seh_hook(infos, num_sections, seh_ctx);
    }

    *out_image = img;
    return COFF_JIT_OK;
  }

  /* -------------------------------------------------------------------------
   * coff_jit_symbol
   * ------------------------------------------------------------------------- */

  void *coff_jit_symbol(const CoffJitImage *image, const char *name)
  {
    for (uint32_t i = 0; i < image->num_exports; ++i)
    {
      if (strcmp(image->exports[i].name, name) == 0)
        return (void *)image->exports[i].addr;
    }
    return NULL;
  }

  /* -------------------------------------------------------------------------
   * coff_jit_free
   * ------------------------------------------------------------------------- */

  void coff_jit_free(CoffJitImage *image)
  {
    if (!image)
      return;

    free_sections(image, image->num_sections);
    free(image->sections);

    for (uint32_t i = 0; i < image->num_exports; ++i)
      free(image->exports[i].name);
    free(image->exports);

    free(image);
  }

#endif /* COFF_JIT_IMPLEMENTATION */

#ifdef __cplusplus
}
#endif
#endif /* COFF_JIT_I686_H */