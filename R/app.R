#' function to launch the nfi app
#'
#' @export
allometr_app <- function() {

  ## database ####
  allometr_db <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = 'guest',
    password = 'guest',
    dbname = 'allometr_db'
  )

  ## tables ####
  allometries_table <- dplyr::tbl(allometr_db, 'ALLOMETRIES') %>% dplyr::collect()
  variables_thesaurus <- dplyr::tbl(allometr_db, 'THESAURUS_VARIABLES') %>% dplyr::collect()
  cubication_thesaurus <- dplyr::tbl(allometr_db, 'THESAURUS_CUBICATION') %>% dplyr::collect()

  ## UI ####
  ui <- shiny::tagList(
    shinyjs::useShinyjs(),
    shiny::fluidPage(

      # title
      shiny::titlePanel('Allometries for everyone!!'),

      # layout
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          width = 2,
          mod_dataInput(
            'mod_dataInput',
            allometries_table, variables_thesaurus, cubication_thesaurus
          )
        ),

        mainPanel = shiny::mainPanel(
          width = 10,

          ########################################################### debug ####
          # shiny::absolutePanel(
          #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
          #   draggable = TRUE, width = 640, height = 'auto',
          #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
          #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
          #   top = 60, left = 'auto', right = 50, bottom = 'auto',
          #
          #   shiny::textOutput('debug1'),
          #   shiny::textOutput('debug2'),
          #   shiny::textOutput('debug3')
          # ),
          ####################################################### end debug ####

          shiny::fluidRow(
            mod_tableOutput(
              'mod_tableOutput',
              allometries_table, variables_thesaurus, cubication_thesaurus
            )
          ),
          shiny::br(),
          shiny::br(),
          mod_equationOutput('mod_equationOutput')
        )
      )
    )
  )

  ## SERVER ####
  server <- function(input, output, session) {

    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      allometries_table, variables_thesaurus, cubication_thesaurus
    )

    table_reactives <- shiny::callModule(
      mod_table, 'mod_tableOutput',
      allometries_table, variables_thesaurus, cubication_thesaurus,
      data_reactives
    )

    shiny::callModule(
      mod_equation, 'mod_equationOutput',
      data_reactives, table_reactives
    )

    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   data_reactives$allolvl
    # })
    # output$debug2 <- shiny::renderPrint({
    #   data_reactives$spatial
    # })
    # output$debug3 <- shiny::renderPrint({
    #   table_reactives$allometries_table_rows_selected
    # })
  }

  # Run the application
  allometr_app_res <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(allometr_db)
      })
    }
  )

  return(allometr_app_res)
}