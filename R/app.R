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

    # shinyjs
    shinyjs::useShinyjs(),
    # shinyWidgets::chooseSliderSkin(skin = "Shiny", color = '#0DB3D4'),
    # shinyWidgets::useSweetAlert(),

    # css
    shiny::tags$head(
      # custom css
      shiny::includeCSS(
        system.file('resources', 'allometrapp.css', package = 'allometrApp')
      )
    ),

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

      # navbarPage contents
      shiny::tabPanel(
        title = 'Explore',
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
        shiny::fluidPage(
          shiny::sidebarLayout(

            sidebarPanel = shiny::sidebarPanel(
              width = 2,
              shiny::h4('Filter the allometries'),
              # mod_dataInput('mod_dataInput'),
              shinyWidgets::selectizeGroupUI(
                id = 'allometries_filters', inline = FALSE,
                params = list(
                  dependent_var = list(inputId = 'dependent_var', title = 'Dependent variable'),
                  independent_var_1 = list(inputId = 'independent_var_1', title = 'Independent variable 1'),
                  independent_var_2 = list(inputId = 'independent_var_2', title = 'Independent variable 2'),
                  independent_var_3 = list(inputId = 'independent_var_3', title = 'Independent variable 3'),
                  allometry_level = list(inputId = 'allometry_level', title = 'Allometry levels'),
                  spatial_level = list(inputId = 'spatial_level', title = 'Spatial levels'),
                  spatial_level_name = list(inputId = 'spatial_level_name', title = 'Spatial level values'),
                  functional_group_level = list(inputId = 'functional_group_level', title = 'Functional group'),
                  functional_group_level_name = list(inputId = 'functional_group_level_name', title = 'Functional group values'),
                  cubication_shape = list(inputId = 'cubication_shape', title = 'Cubication shape'),
                  special_param = list(inputId = 'special_param', title = 'Special parameter')
                )
              ),
              # download buttons
              shiny::h4('Download allometries table'),
              shiny::downloadButton('download_allotable_csv', 'csv'),
              shiny::downloadButton('download_allotable_xlsx', 'xlsx')
            ),
            mainPanel = shiny::mainPanel(
              width = 10,

              # tabset panel
              shiny::tabsetPanel(
                id = 'tabs_panel',

                # table tab
                shiny::tabPanel(
                  'Table',
                  DT::DTOutput('allometr_table')
                ),

                # calculate panel
                shiny::tabPanel(
                  'Calculate',

                  shiny::fluidRow(
                    shiny::column(
                      3,
                      shiny::br(),
                      shinyWidgets::panel(
                        heading = 'Select the data to convert',

                        # panel contents
                        shiny::p(
                          'Please, select a file to load with the data to be converted. ',
                          'Accepted formats are csv and xlsx.',
                          'Both of them must have a header with columns names.'
                        ),
                        shiny::fileInput(
                          'user_data', NULL, FALSE,
                          accept = c(
                            '.csv', '.xlsx', 'text/csv', 'text/comma-separated-values,text/plain',
                            'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                            buttonLabel = 'Browse...', placeholder = 'No file selected...'
                          )
                        ),
                        shiny::p(
                          'Select the allometries to use. If in doubt check the ',
                          shiny::actionLink('link_to_table', 'allometry table')
                        ),
                        shinyWidgets::pickerInput(
                          'allometry_selector', NULL, choices = '', multiple = TRUE,
                          options = shinyWidgets::pickerOptions(
                            size = 5, liveSearch = TRUE
                          )
                        ),
                        shiny::p(
                          'Select the variables from the uploaded data corresponding to the ',
                          'independent variables from the equation:'
                        ),
                        shiny::uiOutput('var_declaration'),
                        # download buttons
                        shiny::h4('Download calculated allometries'),
                        shiny::downloadButton('download_alloresults_csv', 'csv'),
                        shiny::downloadButton('download_alloresults_xlsx', 'xlsx')
                      )
                    ),
                    shiny::column(
                      9,
                      shiny::tableOutput('res_data')
                    )
                  )
                )
              )
            )
          ) # end of sidebar layout
        ) # end of fluidPage
      ) # end of tabPanel "Explore"
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
    alloms_filtered <- shiny::callModule(
      shinyWidgets::selectizeGroupServer, id = 'allometries_filters',
      data = allometries_table,
      vars = c(
        'dependent_var', 'independent_var_1', 'independent_var_2', 'independent_var_3',
        'allometry_level', 'spatial_level', 'spatial_level_name', 'functional_group_level',
        'functional_group_level_name', 'cubication_shape', 'special_param'
      )
    )


    # data inputs
    # data_reactives <- shiny::callModule(
    #   mod_data, 'mod_dataInput',
    #   allometries_table, variables_thesaurus, cubication_thesaurus, lang
    # )

    ## link to table ####
    shiny::observeEvent(
      input$link_to_table,
      {
        shiny::updateTabsetPanel(session, 'tabs_panel', 'Table')
      }
    )

    ## allo table ####
    output$allometr_table <- DT::renderDT({
      alloms_filtered() %>%
        dplyr::mutate_if(is.numeric, round, 3) %>%
        DT::datatable(
          class = 'compact hover nowrap row-border order-column',
          extensions = 'Scroller',
          options = list(
            dom = 'tr',
            # pageLength = 10,
            # lengthMenu = c(10, 25, 50),
            deferRender = TRUE,
            scrollY = '600px', scroller = TRUE, scrollX = TRUE
          )
        )
    })

    ## res table ####
    # left side
    user_data <- shiny::reactive({

      if (is.null(input$user_data)) {
        return(NULL)
      }

      extension <- input$user_data$name %>%
        stringr::str_extract('\\.[a-zA-Z]{3,4}$')
      if (extension == '.csv') {
        res <- readr::read_csv(input$user_data$datapath)
      } else {
        res <- readxl::read_excel(input$user_data$datapath)
      }

      return(res)
    })

    observe({
      id_choices <- alloms_filtered() %>%
        dplyr::pull(allometry_id)

      updateSelectInput(
        session, 'allometry_selector', NULL, id_choices
      )
    })

    output$var_declaration <- renderUI({

      shiny::validate(
        shiny::need(user_data(), 'No user data provided'),
        shiny::need(input$allometry_selector, 'No allometries selected')
      )

      # browser()

      allom_id <- input$allometry_selector
      allom_desc <- allom_description(id = allom_id)

      independent_vars <- allom_desc %>% {

        iv1 <- purrr::map_depth(., 1, 'independent_var_1') %>% purrr::flatten_chr()
        iv2 <- purrr::map_depth(., 1, 'independent_var_2') %>% purrr::flatten_chr()
        iv3 <- purrr::map_depth(., 1, 'independent_var_3') %>% purrr::flatten_chr()

        c(iv1, iv2, iv3) %>%
          unique() %>%
          purrr::discard(function(x) {is.na(x)})
      }

      # independent_vars <- c(
      #   allom_desc[[allom_id]][['independent_var_1']],
      #   allom_desc[[allom_id]][['independent_var_2']],
      #   allom_desc[[allom_id]][['independent_var_3']]
      # ) %>%
      #   purrr::discard(function(x) {is.na(x)})

      lapply(independent_vars, function(x) {
        shinyWidgets::pickerInput(
          glue::glue("{x}_input"), glue::glue("Variable acting as {x}"),
          choices = user_data() %>% dplyr::select_if(is.numeric) %>% names()
        )
      })
    })

    allom_variables_exprs <- reactive({

      # browser()

      allom_id <- input$allometry_selector
      allom_desc <- allom_description(id = allom_id)

      independent_vars <- allom_desc %>% {

        iv1 <- purrr::map_depth(., 1, 'independent_var_1') %>% purrr::flatten_chr()
        iv2 <- purrr::map_depth(., 1, 'independent_var_2') %>% purrr::flatten_chr()
        iv3 <- purrr::map_depth(., 1, 'independent_var_3') %>% purrr::flatten_chr()

        c(iv1, iv2, iv3) %>%
          unique() %>%
          purrr::discard(function(x) {is.na(x)})
      }

      independent_vars %>%
        purrr::walk(
          ~ shiny::validate(
            shiny::need(input[[paste0(.x, '_input')]], 'No variable declaration')
          )
        ) %>%
        purrr::map_chr(
          ~ glue::glue("{.x} = {input[[paste0(.x, '_input')]]},")
        ) %>%
        stringr::str_c(collapse = ' ')

    })

    calculated_data <- reactive({


      shiny::validate(
        shiny::need(user_data(), 'No user data provided'),
        shiny::need(input$allometry_selector, 'No allometries selected')

        # shiny::need(
        #   !is.character(user_data()[['']])
        # )
      )

      shiny::validate(
        shiny::need(allom_variables_exprs(), 'No variables declaration provided'),
        shiny::need(length(allom_variables_exprs()) > 0, 'No variables declaration provided')
      )
      # browser()

      # let's try to do it with glue + parse_exprs because it will be easier I think
      input$allometry_selector %>%
        purrr::map_chr(
          ~ glue::glue(
              "user_data() %>%
                 allom_calculate(
                   {paste0(allom_variables_exprs(), sep = ' ')}
                   allometry_id = '{.x}',
                   # name = '{allom_description(id = .x)[[.x]]$dependent_var}'
                   name = '{.x}'
                 )"
          )
        ) %>%
        rlang::parse_exprs() %>%
        purrr::map(~ eval(.x)) %>%
        purrr::reduce(dplyr::left_join)
    })

    output$res_data <- renderTable({
      calculated_data()
    })

    ## download allo table ####
    output$download_allotable_csv <- downloadHandler(
      filename = function() {
        paste("allometries_table_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {

        data_res <- alloms_filtered()

        readr::write_csv(data_res, file)
      }
    )

    output$download_allotable_xlsx <- downloadHandler(
      filename = function() {
        paste("allometries_table_", Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {

        data_res <- alloms_filtered()

        writexl::write_xlsx(data_res, file)
      }
    )

    ## download res table ####
    output$download_alloresults_csv <- downloadHandler(
      filename = function() {
        paste("calculated_allometry", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {

        data_res <- calculated_data()

        readr::write_csv(data_res, file)
      }
    )

    output$download_alloresults_xlsx <- downloadHandler(
      filename = function() {
        paste("calculated_allometry", Sys.Date(), '.xlsx', sep = '')
      },
      content = function(file) {

        data_res <- calculated_data()

        writexl::write_xlsx(data_res, file)
      }
    )

  } # end of server function

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