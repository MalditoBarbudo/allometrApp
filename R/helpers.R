# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[4]][[1]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

# translate app function
translate_app <- function(id, lang, db) {

  translate_logic <- function(data_filtered, id) {

    if (nrow(data_filtered) < 1) {
      id
    } else {
      dplyr::pull(data_filtered, glue::glue("translation_{lang}"))
    }
  }

  id |>
    purrr::map_chr(
      ~ db$get_data('thesaurus_app') |>
        dplyr::filter(text_id == .x) |>
        translate_logic(.x)
    )
}

get_independent_vars_helper <- function(allom_desc) {
  iv1 <- purrr::map_depth(allom_desc, 1, 'independent_var_1') |> purrr::flatten_chr()
  iv2 <- purrr::map_depth(allom_desc, 1, 'independent_var_2') |> purrr::flatten_chr()
  iv3 <- purrr::map_depth(allom_desc, 1, 'independent_var_3') |> purrr::flatten_chr()

  c(iv1, iv2, iv3) |>
    unique() |>
    purrr::discard(function(x) {is.na(x)})
}