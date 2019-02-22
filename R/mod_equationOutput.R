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
      # previous button
      shiny::column(
        width = 1,
        shiny::br(),
        shinyWidgets::actionBttn(
          ns('equation_prev'), style = 'material-circle', icon = shiny::icon('arrow-circle-left')
        )
      ),
      # equation info
      shiny::column(
        width = 5,
        gt::gt_output(ns('equation_info'))
      ),
      # equation latex
      shiny::column(
        width = 5,
        shiny::uiOutput(ns('equation_latex'))
      ),
      # next button
      shiny::column(
        width = 1,
        shiny::br(),
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
#' @param data_reactives data reactive inputs from mod_dataInput
#' @param table_reactives table reactive inputs from mod_tableOutput
#'
#' @export
mod_equation <- function(
  input, output, session,
  data_reactives, table_reactives
) {

  # for the custom carouse to work we need to establish the index of the equation as
  # a reactive value, and set up observers to change the index. Also we need to know
  # which subgroup of equations we are showing (the rows selected in the table)

  # index
  equation_index <- shiny::reactiveVal(1)

  # resetting the equation_index everytime the table_data is refreshed
  shiny::observeEvent(
    eventExpr = table_reactives$table_data,
    handlerExpr = {
      equation_index(1)
    }
  )

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

  # equation details
  output$equation_info <- gt::render_gt({
    table_reactives$table_data %>%
      # double slice, first the rows selected in the table and after the equation index
      dplyr::slice(table_reactives$allometries_table_rows_selected) %>%
      dplyr::slice(equation_index()) %>%
      # gather and convert to gt
      tidyr::gather('Vars', 'Values') %>%
      gt::gt(rowname_col = 'Vars') %>%
      gt::tab_header(title = 'Detailed equation info')
  })

  # equation latex
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

  ## TODO consider to get rid of the selected rows and allow the carousel to iterate
  ## between all the equations present in the table. The selection load will be
  ## transferred to the filters in the inputs
}