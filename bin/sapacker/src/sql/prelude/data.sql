-- Sets up the Sqlite Database
-- for usage

PRAGMA user_version = 1;
PRAGMA synchronous = OFF;

-- Clear things eagerly
DROP TABLE IF EXISTS Metadata;
DROP TABLE IF EXISTS Bincode;
DROP TABLE IF EXISTS LibFnmap;
DROP TABLE IF EXISTS DllStore;
DROP TABLE IF EXISTS GlobalData;

-- The following identifiers are a MUST to be present
-- 
-- `0` = Last section id
-- `1` = Top priority compile queue
-- `2` = Priority compile queue
CREATE TABLE IF NOT EXISTS Metadata(
  identifier INTEGER PRIMARY KEY,

  sizeof INTEGER NOT NULL,
  valuedata BLOB NOT NULL CHECK (length(valuedata) = sizeof)
) WITHOUT ROWID;

-- Binaries & native libraries that are to be eagerly loaded
CREATE TABLE IF NOT EXISTS Bincode(
  sectionid INTEGER PRIMARY KEY,
  -- '0' ==> Bytecode
  -- '1' ==> Library to be eargerly loaded (i.e. to be loaded on infact 1st call)
  -- '2' ==> Explicit Loader library (i.e. libraries that are explicitly loaded/unloaded)
  assetclass INTEGER NOT NULL,
  -- For explicit loader library or eagerly loaded library, it is simply a u128 data 
  -- 0..63: LibId
  -- 64..127: FuncId
  bindata BLOB NOT NULL,
  CONSTRAINT valid_blob CHECK (
    ((assetclass = 0) OR (length(bindata) = 16)) AND (assetclass >= 0) AND (assetclass <= 2)
  )
) WITHOUT ROWID;

-- Maps a 128-bit virtual address (lib_id + fn_id) to metadata
CREATE TABLE IF NOT EXISTS LibFnmap(
  library_id INTEGER NOT NULL,                                  -- High 64 bits
  function_id INTEGER NOT NULL,                                 -- Low 64 bits
  symbol_name BLOB NOT NULL CHECK (length(symbol_name) <= 512), -- The actual name of the C function (e.g., "sqlite3_open")
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
) WITHOUT ROWID;

-- This is the GLOBAL DATA to be mounted
-- as RW
CREATE TABLE IF NOT EXISTS GlobalData(
  identifier INTEGER PRIMARY KEY DEFAULT 1 CHECK (identifier=1),
  todisk BOOLEAN NOT NULL DEFAULT TRUE,
  globdata BLOB NOT NULL
) WITHOUT ROWID;