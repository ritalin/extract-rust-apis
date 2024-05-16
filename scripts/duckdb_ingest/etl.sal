/* create schema */
.read scripts/duckdb_ingest/00_schema.sql

/* read sources */
.read scripts/duckdb_ingest/01_read_source.sql

/* processing as temporary */
.read scripts/duckdb_ingest/02_process_type.sql
.read scripts/duckdb_ingest/03_process_fn.sql

/* build artifact */
.read scripts/duckdb_ingest/04_buildup.sql

/* export */
.read scripts/duckdb_ingest/05_output.sql
