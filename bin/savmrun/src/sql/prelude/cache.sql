-- Sets up the Sqlite Database
-- for cache store

PRAGMA user_version = 2;

CREATE TABLE IF NOT EXISTS Metadata(
  prim INTEGER PRIMARY KEY DEFAULT 1 CHECK (prim = 1),

  -- Stores the database hash of the stored code
  -- Ensures we don't execute stale code
  --
  -- This is the hash of the sabin, savmrun version and cpu_hash
  -- 
  -- This is forced to be BLAKE3 hash
  valhash BLOB NOT NULL CHECK (length(valhash) == 32)
);

-- We HAVE the RowID to ensure 
-- adding an item does not thrash it all
--
-- Since the JIT will NOT be sequential
-- or transactional
CREATE TABLE Cache(
  sectionid INTEGER NOT NULL,
  optlevel INTEGER NOT NULL,

  picklelibcalls BLOB,
  metamap BLOB,
  machinecode BLOB NOT NULL,

  PRIMARY KEY (sectionid, optlevel)
) WITHOUT ROWID;

-- This is the final form of the code cache
--
-- This the fully linked images of the code SLABS
-- to make it easy to remake the whole compilation pass
-- without invoking linker multiple times
CREATE TABLE EquilibriumCodeSections(
  ops INTEGER PRIMARY KEY,

  relocmap BLOB NOT NULL,
  relsectionmap BLOB NOT NULL,
  
  machinecode BLOB NOT NULL
) WITHOUT ROWID;