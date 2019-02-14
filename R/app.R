#' function to launch the nfi app
#'
#' @importFrom magrittr %>%
#'
#' @export
allometr_app <- function() {

  ## database ####
  allometr_db <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = 'ifn',
    password = rstudioapi::askForPassword('Password for ifn'),
    dbname = 'allometr_db'
  )

  ## UI ####
  ui <- shiny::tagList(
    shiny::fluidPage(

      # title
      shiny::titlePanel('Allometries for everyone!!'),

      # layout
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          width = 3,


        ),

        mainPanel = shiny::mainPanel(
          width = 9,

          shiny::fluidRow(
            DT::DTOutput('eqs_table')
          ),
          shiny::br(),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              gt::gt_output('eq_info')
            ),
            shiny::column(
              width = 6,
              shiny::uiOutput('equation_latex')
            )
          )
        )
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    eq_tbl <- dplyr::tbl(allometr_db, 'ALLOMETRIES') %>% dplyr::collect()

    output$eqs_table <- DT::renderDT({
      eq_tbl %>%
        dplyr::select(spatial_level:equation) %>%
        # dplyr::slice(1:5) %>%
        DT::datatable(selection = list(mode = 'multiple', selected = c(1)))
    })

    output$eq_info <- gt::render_gt(
      eq_tbl %>%
        dplyr::slice(input$eqs_table_rows_selected[1]) %>%
        tidyr::gather('Vars', 'Values') %>%
        gt::gt(rowname_col = 'Vars') %>%
        gt::tab_header(
          title = 'Detailed equation info'
        )
    )

    output$equation_latex <- shiny::renderUI({
      shiny::div(
        style = 'font-size: 2.5em;',
        shiny::withMathJax(
          glue::glue(
            "$$
          {eq_tbl %>% dplyr::slice(input$eqs_table_rows_selected[1]) %>% dplyr::pull(equation)}
          $$"
          )
        )
      )
    })
  }

  # Run the application
  allometr_app_res <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(oracle_db)
      })
    }
  )

  # shiny::runApp(nfi_app)
  return(allometr_app_res)
}