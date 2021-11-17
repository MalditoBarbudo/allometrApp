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

  id %>%
    purrr::map_chr(
      ~ db$get_data('thesaurus_app') %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}