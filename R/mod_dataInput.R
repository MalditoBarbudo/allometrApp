#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#' @param allometr_db pool object to access the allometries db
#'
#' @export
mod_dataInput <- function(id, allometr_db) {
  # ns
  ns <- shiny::NS(id)

  # tables
  allometries_table <- dplyr::tbl(allometr_db, 'ALLOMETRIES') %>% dplyr::collect()
  variables_thesaurus <- dplyr::tbl(allometr_db, 'THESAURUS_VARIABLES') %>% dplyr::collect()

  # choices
  spatial_choices <- allometries_table %>%
    dplyr::pull(spatial_level) %>%
    unique() %>% sort()

  functgroup_choices <- allometries_table %>%
    dplyr::pull(functional_group_level) %>%
    unique() %>% sort()

  depvar_choices <- variables_thesaurus %>%
    dplyr::filter(var_dependent) %>%
    dplyr::pull(var_id) %>%
    magrittr::set_names(
      variables_thesaurus %>%
        dplyr::filter(var_dependent) %>%
        dplyr::pull(translation_eng)
    )

  indepvars_choices <- variables_thesaurus %>%
    dplyr::filter(var_independent) %>%
    dplyr::pull(var_id) %>%
    magrittr::set_names(
      variables_thesaurus %>%
        dplyr::filter(var_independent) %>%
        dplyr::pull(translation_eng)
    )

  # inputs
  shiny::tagList(
    # spatial
    shiny::selectInput(ns('spatial'), 'Spatial ambit', choices = ''),
    shinyjs::hidden(
      shiny::div(
        id = 'spatial_values_div',
        shiny::uiOutput(ns('spatial_values'))
      )
    ),

    # functional group
    shiny::selectInput(ns('functgroup'), 'Functional group', choices = '', multiple = TRUE),
    shinyjs::hidden(
      shiny::div(
        id = 'spatial_values_div',
        shiny::uiOutput(ns('functgroup_values'))
      )
    ),

    # vars
    shiny::selectInput(ns('depvar'), 'Dependent variable', choices = ''),
    shiny::selectInput(ns('indepvars'), 'Independent variables', choices = '', multiple = TRUE),

    # other
    shiny::selectInput(ns('cubication'), 'Cubication shape', choices = ''),
    shiny::selectInput(ns('specialparam'), 'Special parameter', choices = '')
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param allometr_db pool object to access the allometries db
#'
#' @export
mod_data <- function(
  input, output, session,
  allometr_db
) {

}