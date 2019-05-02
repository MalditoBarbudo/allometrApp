# test data
tibble::tibble(
  tree = glue::glue("a_{1:100}") %>% as.character(),
  diametro = rnorm(100, 32, 8),
  altura = rnorm(100, 55, 8),
  volumen_cc = rnorm(100, 23, 2),
  diametro_rama = rnorm(100, 4, 0.2),
  diametro_sc = diametro - rnorm(100, 1, 0.2),
  biomasa_at = rnorm(100, 12, 1)
) %>% {
  test_data <- .
  readr::write_csv(test_data, 'data-raw/test_data.csv')
  writexl::write_xlsx(test_data, 'data-raw/test_data.xlsx')
}
