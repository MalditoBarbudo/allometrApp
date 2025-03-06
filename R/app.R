#' function to launch the allometr app
#'
#' @export
allometr_app <- function() {

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

  ## JS code needed ############################################################
  keep_alive_script <- shiny::HTML(
    "var socket_timeout_interval;
var n = 0;

$(document).on('shiny:connected', function(event) {
  socket_timeout_interval = setInterval(function() {
    Shiny.onInputChange('alive_count', n++)
  }, 10000);
});

$(document).on('shiny:disconnected', function(event) {
  clearInterval(socket_timeout_interval)
});"
  )

  matomo_script <- shiny::HTML(
    "var _paq = window._paq = window._paq || [];
_paq.push(['trackPageView']);
_paq.push(['enableLinkTracking']);
(function() {
  var u='https://stats-emf.creaf.cat/';
  _paq.push(['setTrackerUrl', u+'matomo.php']);
  _paq.push(['setSiteId', '5']);
  var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
  g.async=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
})();

// Event Tracking Code
$(document).on('shiny:inputchanged', function(event) {
  if (/^mod_data*/.test(event.name)) {
    _paq.push(['trackEvent', 'dataInputs', 'updates', event.name, 1, {dimension1: event.value}]);
  }
});"
  )

  ## UI ####
  ui <- shiny::tagList(

    # shinyjs
    shinyjs::useShinyjs(),

    # waiter/hostess
    # waiter::use_waiter(),
    # waiter::use_hostess(),
    # show waiter on load
    # waiter::waiter_show_on_load(
    #   color = '#F8F9FA',
    #   html = waiter::spin_flowers()
    # ),

    # css
    shiny::tags$head(
      # js script,
      shiny::tags$script(keep_alive_script),
      shiny::tags$script(matomo_script),
      # corporate image css
      shiny::includeCSS(
        system.file('apps_css', 'corp_image.css', package = 'lfcdata')
      ),
      # custom css
      shiny::includeCSS(
        system.file('apps_css', 'allometrapp.css', package = 'lfcdata')
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

      # footer
      footer = shiny::tags$footer(
        shiny::fluidRow(
          shiny::column(
            width = 12, align = "right",
            shiny::HTML(glue::glue(
              '<img src="images/emf_white_logo.svg" width="120px" class="d-inline-block" alt="" loading="lazy">
              <img src="images/creaf_white_logo.svg" width="135px" class="d-inline-block" alt="" loading="lazy">
              <span>({lubridate::year(Sys.Date())})</span>'
            ))
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

    # hostess init
    # waiter::waiter_update(
    #   html = shiny::tagList(
    #     shiny::br(), shiny::br(),
    #     waiter::spin_flowers()
    #   )
    # )
    # hostess_init <- waiter::Hostess$new('loader', infinite = TRUE)
    # hostess_init$start()
    # # close init
    # on.exit(hostess_init$close(), add = TRUE)
    # on.exit(waiter::waiter_hide(), add = TRUE)

    ### DB access ################################################################
    allomdb <- lfcdata::allometries()

    ### Variables names inter ####################################################
    # cubication_thesaurus <- allomdb$get_data('thesaurus_cubication')
    # variables_thesaurus <- allomdb$get_data('thesaurus_variables')
    # allometries_table <-
    #   allomdb$get_data('allometries') |>
    #   # dependent var
    #   dplyr::left_join(
    #     variables_thesaurus |>
    #       dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    #     by = c("dependent_var" = "var_id"),
    #     suffix = c("", "_dependent")
    #   ) |>
    #   # independent_var_1
    #   dplyr::left_join(
    #     variables_thesaurus |>
    #       dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    #     by = c("independent_var_1" = "var_id"),
    #     suffix = c("", "_independent_1")
    #   ) |>
    #   # independent_var_2
    #   dplyr::left_join(
    #     variables_thesaurus |>
    #       dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    #     by = c("independent_var_2" = "var_id"),
    #     suffix = c("", "_independent_2")
    #   ) |>
    #   # independent_var_3
    #   dplyr::left_join(
    #     variables_thesaurus |>
    #       dplyr::select(var_id, var_units, dplyr::starts_with('translation')),
    #     by = c("independent_var_3" = "var_id"),
    #     suffix = c("", "_independent_3")
    #   ) |>
    #   dplyr::rename(
    #     dependent_var_units = var_units,
    #     dependent_var_translation_cat = translation_cat,
    #     dependent_var_translation_spa = translation_spa,
    #     dependent_var_translation_eng = translation_eng,
    #     independent_var_1_units = var_units_independent_1,
    #     independent_var_1_translation_cat = translation_cat_independent_1,
    #     independent_var_1_translation_spa = translation_spa_independent_1,
    #     independent_var_1_translation_eng = translation_eng_independent_1,
    #     independent_var_2_units = var_units_independent_2,
    #     independent_var_2_translation_cat = translation_cat_independent_2,
    #     independent_var_2_translation_spa = translation_spa_independent_2,
    #     independent_var_2_translation_eng = translation_eng_independent_2,
    #     independent_var_3_units = var_units_independent_3,
    #     independent_var_3_translation_cat = translation_cat_independent_3,
    #     independent_var_3_translation_spa = translation_spa_independent_3,
    #     independent_var_3_translation_eng = translation_eng_independent_3
    #   )



    ## explore UI (to use lang) ####
    output$explore_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      # proper UI
      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 3,
            shiny::h4(translate_app('sidebar_filter_h4', lang_declared)),
            mod_dataInput(
              id = 'allometries_filters', inline = FALSE,
              params = list(
                dependent_var = list(inputId = 'dependent_var', title = translate_app('dependent_var', lang_declared)),
                independent_var_1 = list(inputId = 'independent_var_1', title = translate_app('independent_var_1', lang_declared)),
                independent_var_2 = list(inputId = 'independent_var_2', title = translate_app('independent_var_2', lang_declared)),
                # independent_var_3 = list(inputId = 'independent_var_3', title = translate_app# dependent_var', lang_declared)),
                allometry_level = list(inputId = 'allometry_level', title = translate_app('allometry_level', lang_declared)),
                spatial_level = list(inputId = 'spatial_level', title = translate_app('spatial_level', lang_declared)),
                spatial_level_name = list(inputId = 'spatial_level_name', title = translate_app('spatial_level_name', lang_declared)),
                functional_group_level = list(inputId = 'functional_group_level', title = translate_app('functional_group_level', lang_declared)),
                functional_group_level_name = list(inputId = 'functional_group_level_name', title = translate_app('functional_group_level_name', lang_declared)),
                cubication_shape = list(inputId = 'cubication_shape', title = translate_app('cubication_shape', lang_declared)),
                special_param = list(inputId = 'special_param', title = translate_app('special_param', lang_declared))
              ),
              btn_label = translate_app("reset_all", lang_declared)
            ),
            # download buttons
            shiny::h4(translate_app('sidebar_download_h4', lang_declared)),
            shiny::downloadButton('download_allotable_csv', 'csv'),
            shiny::downloadButton('download_allotable_xlsx', 'xlsx')
          ),
          mainPanel = shiny::mainPanel(
            width = 9,

            # tabset panel
            shiny::tabsetPanel(
              id = 'tabs_panel',

              # table tab
              shiny::tabPanel(
                translate_app('table_tab_title', lang_declared),
                DT::DTOutput('allometr_table')
              ),

              # calculate panel
              shiny::tabPanel(
                translate_app('calculate_tab_title', lang_declared),

                shiny::fluidRow(
                  shiny::column(
                    3,
                    shiny::br(),
                    shinyWidgets::panel(
                      heading = translate_app('calculate_panel_heading', lang_declared),

                      # panel contents
                      shiny::p(
                        translate_app('calculate_panel_upload_p', lang_declared)
                      ),
                      shiny::fileInput(
                        'user_data', NULL, FALSE,
                        accept = c(
                          '.csv', '.xlsx', 'text/csv', 'text/comma-separated-values,text/plain',
                          'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
                        ),
                        buttonLabel = translate_app('user_data_button_label', lang_declared),
                        placeholder = translate_app('user_data_button_placeholder', lang_declared)
                      ),
                      shiny::p(
                        translate_app('calculate_panel_allosel_p', lang_declared),
                        shiny::actionLink('link_to_table', translate_app('calculate_panel_allotable_link', lang_declared))
                      ),
                      shinyWidgets::pickerInput(
                        'allometry_selector', NULL, choices = '', multiple = TRUE,
                        options = shinyWidgets::pickerOptions(
                          size = 5, liveSearch = TRUE,
                          noneSelectedText = translate_app('nothing_selected', lang_declared)
                        )
                      ),
                      shiny::p(
                        translate_app('calculate_panel_vardec_p', lang_declared)
                      ),
                      shiny::uiOutput('var_declaration'),
                      # download buttons
                      shiny::h4(translate_app('calculate_panel_download_h4', lang_declared)),
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
      mod_data, id = 'allometries_filters', data = allometries_table,
      lang = lang,
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
          session, 'tabs_panel', translate_app('table_tab_title', lang())
        )
      }
    )

    ## allo table ####
    # TODO change table headers to the lang
    output$allometr_table <- DT::renderDT({

      lang_declared <- lang()

      alloms_filtered() |>
        dplyr::mutate_if(is.numeric, round, 3) |>
        purrr::set_names(translate_app(names(alloms_filtered()), lang_declared)) |>
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

      extension <- input$user_data$name |>
        stringr::str_extract('\\.[a-zA-Z]{3,4}$')
      if (extension == '.csv') {
        res <- readr::read_csv(input$user_data$datapath)
      } else {
        res <- readxl::read_excel(input$user_data$datapath)
      }

      return(res)
    })

    observe({
      id_choices <- alloms_filtered() |>
        dplyr::pull(allometry_id)

      shinyWidgets::updatePickerInput(
        session, 'allometry_selector', NULL, choices = id_choices
      )
    })

    output$var_declaration <- renderUI({

      shiny::validate(
        shiny::need(user_data(), translate_app('need_user_data', lang())),
        shiny::need(input$allometry_selector, translate_app('need_allosel', lang()))
      )

      allom_id <- input$allometry_selector
      independent_vars <- get_independent_vars_helper(allom_id)

      lapply(independent_vars, function(x) {

        units <- variables_thesaurus |>
          dplyr::filter(var_id == x) |>
          dplyr::pull(var_units)

        shinyWidgets::pickerInput(
          glue::glue("{x}_input"), glue::glue(translate_app('calculate_panel_vardec_inputs', lang())),
          choices = user_data() |> dplyr::select_if(is.numeric) |> names()
        )
      })
    })

    allom_variables_exprs <- reactive({

      # browser()

      allom_id <- input$allometry_selector
      independent_vars <- get_independent_vars_helper(allom_id)

      independent_vars |>
        purrr::walk(
          ~ shiny::validate(
            shiny::need(input[[paste0(.x, '_input')]], translate_app('need_vardec', lang()))
          )
        ) |>
        purrr::map_chr(
          ~ glue::glue("user_data()[['{input[[paste0(.x, '_input')]]}']]")
        ) |>
        rlang::parse_exprs() |>
        rlang::set_names(independent_vars)

    })

    calculated_data <- reactive({


      shiny::validate(
        shiny::need(
          user_data(),
          translate_app('need_user_data', lang())
        ),
        shiny::need(
          input$allometry_selector,
          translate_app('need_allosel', lang())
        ),
        shiny::need(
          allom_variables_exprs(),
          translate_app('need_vardec', lang())
        ),
        shiny::need(
          length(allom_variables_exprs()) > 0,
          translate_app('need_vardec', lang())
        )
      )

      # let's use the calculate method in lfcdata
      res_calculation <- input$allometry_selector |>
        purrr::map_dfc(
          ~ allomdb$calculate(
            !!! allom_variables_exprs(),
            allometry_id = .x
          )
        ) |>
        rlang::set_names(input$allometry_selector)

      dplyr::bind_cols(user_data(), res_calculation)
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
    ui = ui, server = server
  )

  # shiny::runApp(nfi_app)
  return(allometrApp)
}
