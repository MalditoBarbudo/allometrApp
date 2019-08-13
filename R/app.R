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
  # allometries_table <- dplyr::tbl(allometr_db, tolower('ALLOMETRIES')) %>%
  #   dplyr::collect()
  # variables_thesaurus <- dplyr::tbl(allometr_db, tolower('THESAURUS_VARIABLES')) %>%
  #   dplyr::collect()
  cubication_thesaurus <- dplyr::tbl(allometr_db, tolower('THESAURUS_CUBICATION')) %>%
    dplyr::collect()
  # data("allometries_table")
  # data("variables_thesaurus")

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

        # we need an UI beacuse we need to translate based on the lang input from the
        # navbar
        shiny::uiOutput('explore_ui')

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

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    ## explore UI (to use lang) ####
    output$explore_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      # proper UI
      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 2,
            shiny::h4(translate_app('sidebar_filter_h4', lang_declared, allometr_db)),
            mod_dataInput(
              id = 'allometries_filters', inline = FALSE,
              params = list(
                dependent_var = list(inputId = 'dependent_var', title = translate_app('dependent_var', lang_declared, allometr_db)),
                independent_var_1 = list(inputId = 'independent_var_1', title = translate_app('independent_var_1', lang_declared, allometr_db)),
                independent_var_2 = list(inputId = 'independent_var_2', title = translate_app('independent_var_2', lang_declared, allometr_db)),
                # independent_var_3 = list(inputId = 'independent_var_3', title = translate_app# dependent_var', lang_declared, allometr_db)),
                allometry_level = list(inputId = 'allometry_level', title = translate_app('allometry_level', lang_declared, allometr_db)),
                spatial_level = list(inputId = 'spatial_level', title = translate_app('spatial_level', lang_declared, allometr_db)),
                spatial_level_name = list(inputId = 'spatial_level_name', title = translate_app('spatial_level_name', lang_declared, allometr_db)),
                functional_group_level = list(inputId = 'functional_group_level', title = translate_app('functional_group_level', lang_declared, allometr_db)),
                functional_group_level_name = list(inputId = 'functional_group_level_name', title = translate_app('functional_group_level_name', lang_declared, allometr_db)),
                cubication_shape = list(inputId = 'cubication_shape', title = translate_app('cubication_shape', lang_declared, allometr_db)),
                special_param = list(inputId = 'special_param', title = translate_app('special_param', lang_declared, allometr_db))
              )
            ),
            # download buttons
            shiny::h4(translate_app('sidebar_download_h4', lang_declared, allometr_db)),
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
                translate_app('table_tab_title', lang_declared, allometr_db),
                DT::DTOutput('allometr_table')
              ),

              # calculate panel
              shiny::tabPanel(
                translate_app('calculate_tab_title', lang_declared, allometr_db),

                shiny::fluidRow(
                  shiny::column(
                    3,
                    shiny::br(),
                    shinyWidgets::panel(
                      heading = translate_app('calculate_panel_heading', lang_declared, allometr_db),

                      # panel contents
                      shiny::p(
                        translate_app('calculate_panel_upload_p', lang_declared, allometr_db)
                      ),
                      shiny::fileInput(
                        'user_data', NULL, FALSE,
                        accept = c(
                          '.csv', '.xlsx', 'text/csv', 'text/comma-separated-values,text/plain',
                          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
                        ),
                        buttonLabel = translate_app('user_data_button_label', lang_declared, allometr_db),
                        placeholder = translate_app('user_data_button_placeholder', lang_declared, allometr_db)
                      ),
                      shiny::p(
                        translate_app('calculate_panel_allosel_p', lang_declared, allometr_db),
                        shiny::actionLink('link_to_table', translate_app('calculate_panel_allotable_link', lang_declared, allometr_db))
                      ),
                      shinyWidgets::pickerInput(
                        'allometry_selector', NULL, choices = '', multiple = TRUE,
                        options = shinyWidgets::pickerOptions(
                          size = 5, liveSearch = TRUE,
                          noneSelectedText = translate_app('nothing_selected', lang_declared, allometr_db)
                        )
                      ),
                      shiny::p(
                        translate_app('calculate_panel_vardec_p', lang_declared, allometr_db)
                      ),
                      shiny::uiOutput('var_declaration'),
                      # download buttons
                      shiny::h4(translate_app('calculate_panel_download_h4', lang_declared, allometr_db)),
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
    })

    ## module calling ####
    alloms_filtered <- shiny::callModule(
      mod_data, id = 'allometries_filters', data = allometrApp::allometries_table,
      lang = lang, db = allometr_db,
      vars = c(
        'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
        'allometry_level', 'spatial_level', 'spatial_level_name', 'functional_group_level',
        'functional_group_level_name', 'cubication_shape', 'special_param'
      )
    )

    ## link to table ####
    shiny::observeEvent(
      input$link_to_table,
      {
        shiny::updateTabsetPanel(
          session, 'tabs_panel', translate_app('table_tab_title', lang(), allometr_db)
        )
      }
    )

    ## allo table ####
    # TODO change table headers to the lang
    output$allometr_table <- DT::renderDT({

      lang_declared <- lang()

      alloms_filtered() %>%
        dplyr::mutate_if(is.numeric, round, 3) %>%
        magrittr::set_colnames(
          translate_app(names(.), lang_declared, allometr_db)
        ) %>%
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
        shiny::need(user_data(), translate_app('need_user_data', lang(), allometr_db)),
        shiny::need(input$allometry_selector, translate_app('need_allosel', lang(), allometr_db))
      )

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

        units <- allometrApp::variables_thesaurus %>%
          dplyr::filter(var_id == x) %>%
          dplyr::pull(var_units)

        shinyWidgets::pickerInput(
          glue::glue("{x}_input"), glue::glue(translate_app('calculate_panel_vardec_inputs', lang(), allometr_db)),
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
            shiny::need(input[[paste0(.x, '_input')]], translate_app('need_vardec', lang(), allometr_db))
          )
        ) %>%
        purrr::map_chr(
          ~ glue::glue("{.x} = {input[[paste0(.x, '_input')]]},")
        ) %>%
        stringr::str_c(collapse = ' ')

    })

    calculated_data <- reactive({


      shiny::validate(
        shiny::need(user_data(), translate_app('need_user_data', lang(), allometr_db)),
        shiny::need(input$allometry_selector, translate_app('need_allosel', lang(), allometr_db))

        # shiny::need(
        #   !is.character(user_data()[['']])
        # )
      )

      shiny::validate(
        shiny::need(allom_variables_exprs(), translate_app('need_vardec', lang(), allometr_db)),
        shiny::need(length(allom_variables_exprs()) > 0, translate_app('need_vardec', lang(), allometr_db))
      )

      # let's try to do it with glue + parse_exprs because it will be easier I think
      input$allometry_selector %>%
        purrr::map_chr(
          ~ glue::glue(
              "user_data() %>%
                 allom_calculate(
                   {paste0(allom_variables_exprs(), sep = ' ')}
                   allometry_id = '{.x}',
                   # name = '{allom_description(id = .x)[[.x]]$dependent_var}'
                   name = '{.x} [{allometrApp::variables_thesaurus %>%
                                  dplyr::filter(var_id == stringr::str_split(.x, '_') %>% purrr::flatten_chr() %>% magrittr::extract(1)) %>%
                                  dplyr::pull(var_units)}]'
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
        paste("calculated_allometries_", Sys.Date(), '.csv', sep = '')
      },
      content = function(file) {

        data_res <- calculated_data()

        readr::write_csv(data_res, file)
      }
    )

    output$download_alloresults_xlsx <- downloadHandler(
      filename = function() {
        paste("calculated_allometries_", Sys.Date(), '.xlsx', sep = '')
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