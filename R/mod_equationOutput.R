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

    # carousel controls
    shiny::fluidRow(
      shiny::column(
        width = 1, offset = 4,
        shinyWidgets::actionBttn(
          ns('equation_prev'), style = 'material-circle', icon = shiny::icon('arrow-circle-left')
        )
      ),
      shiny::column(
        width = 2,
        shiny::uiOutput(ns('carousel_indicator'))
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(
          ns('equation_next'), style = 'material-circle', icon = shiny::icon('arrow-circle-right')
        )
      )
    ),

    # some vertical space
    shiny::br(),
    shiny::br(),

    # carousel contents
    shiny::fluidRow(
      # equation info
      shiny::column(
        width = 6,
        gt::gt_output(ns('equation_info'))
      ),
      # equation latex
      shiny::column(
        width = 6,
        shiny::uiOutput(ns('equation_latex'))
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

  # carousel indicator, to see the index and the total
  output$carousel_indicator <- shiny::renderUI({
    glue::glue("{equation_index()} / {nrow(table_reactives$table_data)}")
  })

  # equations list
  equation_list <- shiny::reactive({
    table_reactives$table_data %>%
      dplyr::pull(equation) #%>%
      # but only those selected
      # magrittr::extract(table_reactives$allometries_table_rows_selected)
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
      # dplyr::slice(table_reactives$allometries_table_rows_selected) %>%
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

  ## TODO Color the table with the visualized equation
  ## TODO Improve the look of the carousel controls
}