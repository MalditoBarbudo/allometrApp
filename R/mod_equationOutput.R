#' @title mod_equationOutput and mod_equation
#'
#' @description A shiny module to create and populate the equation output
#'
#' @param id shiny id
#'
#' @export
mod_equationOutput <- function(id) {

  # ns
  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    shiny::uiOutput(ns('equation_latex'))
  )
}

#' mod_equation server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param allometries_table table with allometries info from allometr_db
#' @param variables_thesaurus table with variables info from allometr_db
#' @param cubication_thesaurus table with cubication info from allometr_db
#' @param data_reactives data reactive inputs from mod_dataInput
#' @param table_reactives table reactive inputs from mod_tableOutput
#'
#' @export
mod_equation <- function(
  input, output, session,
  allometries_table, variables_thesaurus, cubication_thesaurus,
  data_reactives, table_reactives
) {

  # equation
  output$equation_latex <- shiny::renderUI({

    browser()

    shiny::div(
      style = 'font-size: 2.5em;',
      shiny::withMathJax(
        glue::glue(
        "$$
        {allometries_table %>%
          dplyr::slice(table_reactives$eqs_table_rows_selected[1]) %>%
          dplyr::pull(equation)}
        $$"
        )
      )
    )
  })

  ## TODO check this out to see how to do it manually:
  ## https://stackoverflow.com/questions/51531743/image-slideshow-in-r-shiny
  ## THe idea is build a carousel with the equations selected, and also
  ## maybe paint the row with the equation visualized in the table, idk

}