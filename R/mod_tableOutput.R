#' @title mod_tableOutput and mod_table
#'
#' @description A shiny module to create and populate the table output
#'
#' @param id shiny id
#' @param allometries_table table with allometries info from allometr_db
#' @param variables_thesaurus table with variables info from allometr_db
#' @param cubication_thesaurus table with cubication info from allometr_db
#'
#' @export
mod_tableOutput <- function(
  id, allometries_table, variables_thesaurus, cubication_thesaurus
) {

  # ns
  ns <- shiny::NS(id)

  # ui
  shiny::tagList(
    DT::DTOutput(ns('allometries_table'))
  )
}

#' mod_table server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param allometries_table table with allometries info from allometr_db
#' @param variables_thesaurus table with variables info from allometr_db
#' @param cubication_thesaurus table with cubication info from allometr_db
#' @param data_reactives data reactive inputs from mod_dataInput
#'
#' @export
mod_table <- function(
  input, output, session,
  allometries_table, variables_thesaurus, cubication_thesaurus, data_reactives
) {

  # table output
  output$allometries_table <- DT::renderDT({
    allometries_table %>%
      # filtering step, based on the data inputs
      dplyr::filter(!!! data_reactives$filtering_expr) %>%
      DT::datatable(
        selection = list(mode = 'multiple')
      )
  })

  # reactive values to store the selected rows in the table
  table_inputs <- shiny::reactiveValues()
  shiny::observe({
    table_inputs$allometries_table_rows_selected <- input$allometries_table_rows_selected
  })

  # return the inputs
  return(table_inputs)
}