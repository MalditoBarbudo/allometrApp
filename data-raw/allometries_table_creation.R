# database
allometr_db <- pool::dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'guest',
  password = 'guest',
  dbname = 'allometr_db',
  host = 'localhost'
)

# tables
allometries_table <- dplyr::tbl(allometr_db, 'ALLOMETRIES') %>% dplyr::collect()

# close
pool::poolClose(allometr_db)

# use_data
usethis::use_data(
  allometries_table, internal = TRUE, overwrite = TRUE
)