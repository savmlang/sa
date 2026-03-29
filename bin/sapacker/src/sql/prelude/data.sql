-- Sets up the Sqlite Database
-- for usage

PRAGMA user_version = 1;
PRAGMA synchronous = OFF;
PRAGMA journal_mode = OFF;

-- Clear things eagerly
DROP TABLE IF EXISTS Metadata;
DROP TABLE IF EXISTS Bincode;
DROP TABLE IF EXISTS LibFnmap;
DROP TABLE IF EXISTS DllStore;

-- Architectural Features
---
-- # Expanded Mode
-- While enabled, the informs that VM that all are DLLs
-- defined in `DllStore` has to extracted to disk
-- at the location `{SABIN_FILE_PARENT_DIR}/lib` using
-- the naming scheme `{lib prefix}{LIBRARY_ID}{lib extension}`
--
-- Else, as usual, it leads to startup degradation as the VM
-- spends time extracting all the DllStore (every single entry on platform!!)
-- into the VM Runtime Tmp directory (%TEMP%/savmcaches/*)
--
-- # CWD Cache Mode
-- Changes VM Runtime Tmp directory to %CWD%/savmcaches/*


-- The following identifiers are a MUST to be present
-- 
-- `0` = Last section id
-- `1` = Top priority compile queue (array of u64s)
-- `2` = Priority compile queue (array of u64s)
-- `3` = GlobalData
-- `4` = Expanded Mode (BOOL MODE)
-- `5` = CWD Cache Mode (BOOL MODE)
--
-- ## Bool Mode
-- FALSE if `(valuedata as &[u8])[0] == 0u8`
-- TRUE otherwise
CREATE TABLE IF NOT EXISTS Metadata(
  identifier INTEGER PRIMARY KEY,

  valuedata BLOB NOT NULL CHECK (length(valuedata) <= 10485760)
) WITHOUT ROWID;

-- Binaries & native libraries that are to be eagerly loaded
CREATE TABLE IF NOT EXISTS Bincode(
  sectionid INTEGER PRIMARY KEY,
  -- '0' ==> Bytecode
  -- '1' ==> Library to be eargerly loaded (i.e. to be loaded on infact 1st call)
  assetclass INTEGER NOT NULL,
  -- For eagerly loaded library, it is simply a u128 data 
  -- 0..63: LibId (first 8 bytes)
  -- 64..127: FuncId (next 8 bytes)
  -- The encoding is LE
  bindata BLOB NOT NULL,
  CONSTRAINT valid_blob CHECK (
    ((assetclass = 0) OR (length(bindata) = 16)) AND (assetclass >= 0) AND (assetclass <= 2)
  )
) WITHOUT ROWID;

-- Maps a 128-bit virtual address (lib_id + fn_id) to metadata
CREATE TABLE IF NOT EXISTS LibFnmap(
  library_id INTEGER NOT NULL,
  function_id INTEGER NOT NULL,
  symbol_name BLOB NOT NULL CHECK (length(symbol_name) <= 512), -- The actual name of the C function (e.g., "sqlite3_open")
  -- Calling ABI Definition:
  -- Otherwise is parsed as a valid 
  callsig BLOB NOT NULL CHECK (length(callsig) <= 10240),

  PRIMARY KEY (library_id, function_id)
) WITHOUT ROWID;

CREATE TABLE IF NOT EXISTS DllStore(
  library_id INTEGER NOT NULL,
  -- The platform (in Rust's target triple!)
  platform TEXT NOT NULL CHECK (length(platform) <= 48),
  -- The content of the real (literally real)
  -- platform specific dynamic library file
  -- 
  -- This is machine code with the OS headers, not a lcoation
  dylibcontent BLOB NOT NULL,
  PRIMARY KEY (library_id, platform)
);
