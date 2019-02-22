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
  # let's do a carousel (code inspired from this answer
  # https://stackoverflow.com/questions/51531743/image-slideshow-in-r-shiny)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(
          ns('equation_prev'), style = 'material-circle', icon = shiny::icon('arrow-circle-left')
        )
      ),
      shiny::column(
        width = 10,
        shiny::uiOutput(ns('equation_latex'))
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(
          ns('equation_next'), style = 'material-circle', icon = shiny::icon('arrow-circle-right')
        )
      )
    )
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

  # for the custom carouse to work we need to establish the index of the equation as
  # a reactive value, and set up observers to change the index. Also we need to know
  # which subgroup of equations we are showing (the rows selected in the table)

  # index
  equation_index <- shiny::reactiveVal(1)

  # equations list
  equation_list <- shiny::reactive({
    table_reactives$table_data %>%
      dplyr::pull(equation) %>%
      # but only those selected
      magrittr::extract(table_reactives$allometries_table_rows_selected)
  })

  # observers for previous and next
  shiny::observeEvent(
    eventExpr = input[['equation_prev']],
    handlerExpr = {
      equation_index(max(equation_index() - 1, 1))
    }
  )

  shiny::observeEvent(
    eventExpr = input[['equation_next']],
    handlerExpr = {
      equation_index(min(equation_index() + 1, length(equation_list())))
    }
  )

  # equation
  output$equation_latex <- shiny::renderUI({

    equation_selected <- equation_list()[equation_index()]

    shiny::div(
      style = 'font-size: 2.5em;',
      shiny::withMathJax(
        glue::glue(
        "$${equation_selected}$$"
        )
      )
    )
  })
}