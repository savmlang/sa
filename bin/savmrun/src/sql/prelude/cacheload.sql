-- Cacheload data information

PRAGMA journal_mode = WAL;
PRAGMA journal_size_limit = 67108864; -- 64MB

PRAGMA cache_size = -64000;  -- 64MB
PRAGMA page_size = 4096;
PRAGMA mmap_size = 67108864; -- 64MB Memory Mapping Cap

PRAGMA temp_store = MEMORY;
PRAGMA locking_mode = NORMAL;

PRAGMA auto_vacuum = INCREMENTAL;