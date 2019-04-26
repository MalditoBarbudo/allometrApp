# test data
tibble::tibble(
  tree = glue::glue("a_{1:100}") %>% as.character(),
  diametro = rnorm(100, 32, 8),
  altura = rnorm(100, 55, 8)
) %>% {
  test_data <- .
  readr::write_csv(test_data, 'data-raw/test_data.csv')
  writexl::write_xlsx(test_data, 'data-raw/test_data.xlsx')
}
