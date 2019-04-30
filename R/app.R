# #' function to launch the nfi app
# #'
# #' @param file character indicating the flexdashboard Rmd file to load
# #' @param shiny_args list with the shiny arguments for runApp
# #' @param render_params list of named parameters for the Rmd file
# #'
# #' @export
# allometr_app <- function(
#   file,
#   shiny_args = list(host = '0.0.0.0', port = 3838),
#   render_params =  list(
#     appdb_user = 'guest',
#     appdb_password = 'guest',
#     appdb_dbname = 'allometr_db',
#     appdb_host = 'localhost'
#   )
# ) {
#
#   # wrapper for rmarkdown::run with the corresponding arguments
#   rmarkdown::run(
#     file = file, shiny_args = shiny_args, render_args = list(params = render_params)
#   )
# }

#' function to launch the allometr app
#'
#' @importFrom magrittr %>%
#'
#' @export
allometr_app <- function(
  user = 'guest', password = 'guest',
  host = NULL, port = NULL, dbname = 'allometr_db'
) {

  ### DB access ################################################################
  allometr_db <- pool::dbPool(
    RPostgreSQL::PostgreSQL(),
    user = user,
    password = password,
    dbname = dbname,
    host = host,
    port = port
  )

  ### Variables names inter ####################################################
  allometries_table <- dplyr::tbl(allometr_db, 'ALLOMETRIES') %>%
    dplyr::collect()
  variables_thesaurus <- dplyr::tbl(allometr_db, 'THESAURUS_VARIABLES') %>%
    dplyr::collect()
  cubication_thesaurus <- dplyr::tbl(allometr_db, 'THESAURUS_CUBICATION') %>%
    dplyr::collect()

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'allometrApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(

    shinyjs::useShinyjs(),
    # shinyWidgets::chooseSliderSkin(skin = "Shiny", color = '#0DB3D4'),
    # shinyWidgets::useSweetAlert(),

    navbarPageWithInputs(
      # opts
      title = 'AllometrApp',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # custom css
      # shiny::tags$head(
      #   # custom css
      #   shiny::includeCSS(
      #     system.file('resources', 'allometr.css', package = 'allometrApp')
      #   )
      # )

      # navbarPage contents
      shiny::tabPanel(
        title = '',
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
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            mod_dataInput('mod_dataInput')
          ),
          mainPanel = shiny::mainPanel(
            width = 9
          )



        )
      )


    ) # end of navbarPage
  )

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   data_reactives$diameter_classes
    # })
    # output$debug2 <- shiny::renderPrint({
    #   map_reactives$map_click
    # })
    # output$debug3 <- shiny::renderPrint({
    #   map_reactives$map_shape_click
    # })

    # lang reactive
    lang <- shiny::reactive({
      input$lang
    })

    ## module calling ####
    # data inputs
    data_reactives <- shiny::callModule(
      mod_data, 'mod_dataInput',
      allometr_table, variables_thesaurus, cubication_thesaurus, lang
    )
  }

  # Run the application
  allometrApp <- shiny::shinyApp(
    ui = ui, server = server,
    onStart = function() {

      ## on stop routine to cloose the db pool
      shiny::onStop(function() {
        pool::poolClose(allometr_db)
      })
    }
  )

  # shiny::runApp(nfi_app)
  return(allometrApp)

}