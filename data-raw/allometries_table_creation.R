# database
allometr_db <- pool::dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'guest',
  password = 'guest',
  dbname = 'allometr_db',
  host = 'localhost'
)

# tables
variables_thesaurus <- dplyr::tbl(allometr_db, tolower('THESAURUS_VARIABLES')) %>% dplyr::collect()


allometries_table <- dplyr::tbl(allometr_db, tolower('ALLOMETRIES')) %>%
  dplyr::collect() %>%
  # dependent var
  dplyr::left_join(
    variables_thesaurus %>% dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    by = c("dependent_var" = "var_id"),
    suffix = c("", "_dependent")
  ) %>%
  # independent_var_1
  dplyr::left_join(
    variables_thesaurus %>% dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    by = c("independent_var_1" = "var_id"),
    suffix = c("", "_independent_1")
  ) %>%
  # independent_var_2
  dplyr::left_join(
    variables_thesaurus %>% dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    by = c("independent_var_2" = "var_id"),
    suffix = c("", "_independent_2")
  ) %>%
  # independent_var_3
  dplyr::left_join(
    variables_thesaurus %>% dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    by = c("independent_var_3" = "var_id"),
    suffix = c("", "_independent_3")
  ) %>%
  dplyr::rename(
    dependent_var_units = var_units,
    dependent_var_translation_cat = translation_cat,
    dependent_var_translation_spa = translation_spa,
    dependent_var_translation_eng = translation_eng,
    independent_var_1_units = var_units_independent_1,
    independent_var_1_translation_cat = translation_cat_independent_1,
    independent_var_1_translation_spa = translation_spa_independent_1,
    independent_var_1_translation_eng = translation_eng_independent_1,
    independent_var_2_units = var_units_independent_2,
    independent_var_2_translation_cat = translation_cat_independent_2,
    independent_var_2_translation_spa = translation_spa_independent_2,
    independent_var_2_translation_eng = translation_eng_independent_2,
    independent_var_3_units = var_units_independent_3,
    independent_var_3_translation_cat = translation_cat_independent_3,
    independent_var_3_translation_spa = translation_spa_independent_3,
    independent_var_3_translation_eng = translation_eng_independent_3
  )


# close
pool::poolClose(allometr_db)

# use_data
usethis::use_data(
  allometries_table, internal = FALSE, overwrite = TRUE
)
