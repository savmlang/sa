-- RO Specifics
PRAGMA query_only = ON;
PRAGMA journal_mode = OFF;
PRAGMA synchronous = OFF;

-- Keep 10MB worth of Cache in size
PRAGMA cache_size = -2048;
PRAGMA mmap_size = 102400;
PRAGMA temp_store = MEMORY;
PRAGMA locking_mode = NORMAL;
