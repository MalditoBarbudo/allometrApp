#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs
#'
#' @param id shiny id
#' @param allometries_table table with allometries info from allometr_db
#' @param variables_thesaurus table with variables info from allometr_db
#' @param cubication_thesaurus table with cubication info from allometr_db
#'
#' @export
mod_dataInput <- function(
  id, allometries_table, variables_thesaurus, cubication_thesaurus
) {
  # ns
  ns <- shiny::NS(id)

  # choices
  allometry_choices <- allometries_table %>%
    dplyr::pull(allometry_level) %>%
    unique() %>% sort()

  spatial_choices <- allometries_table %>%
    dplyr::pull(spatial_level) %>%
    unique() %>% sort()

  functgroup_choices <- allometries_table %>%
    dplyr::pull(functional_group_level) %>%
    unique() %>% sort()

  depvar_choices <- variables_thesaurus %>%
    dplyr::filter(var_dependent) %>%
    dplyr::pull(var_id) %>%
    magrittr::set_names(
      variables_thesaurus %>%
        dplyr::filter(var_dependent) %>%
        dplyr::pull(translation_eng)
    )

  indepvars_choices <- variables_thesaurus %>%
    dplyr::filter(var_independent) %>%
    dplyr::pull(var_id) %>%
    magrittr::set_names(
      variables_thesaurus %>%
        dplyr::filter(var_independent) %>%
        dplyr::pull(translation_eng)
    )

  cubication_choices <- cubication_thesaurus %>%
    dplyr::pull(cubication_shape_id) %>%
    c('any', .) #%>%
    # magrittr::set_names(
    #   cubication_thesaurus %>%
    #     dplyr::pull(translation_eng) %>%
    #     c('Any', .)
    # )

  specialparam_choices <- allometries_table %>%
    dplyr::pull(special_param) %>% unique() %>% sort()

  # inputs
  shiny::tagList(
    # allometry
    shiny::selectInput(
      ns('allolvl'), 'Allometry level',
      choices = allometry_choices, multiple = TRUE
    ),

    # spatial
    shiny::selectInput(
      ns('spatial'), 'Spatial ambit', choices = spatial_choices, multiple = TRUE
    ),
    shinyjs::hidden(
      shiny::uiOutput(ns('spatial_values'))
    ),

    # functional group
    shiny::selectInput(
      ns('functgroup'), 'Functional group', choices = functgroup_choices,
      multiple = TRUE
    ),
    shinyjs::hidden(
      shiny::div(
        id = ns('functgroup_values_div'),
        shiny::uiOutput(ns('functgroup_values'))
      )
    ),

    # vars
    shiny::selectInput(
      ns('depvar'), 'Dependent variable',
      choices = depvar_choices, multiple = TRUE
    ),
    shiny::selectInput(
      ns('indepvars'), 'Independent variables',
      choices = indepvars_choices, multiple = TRUE
    ),

    # other
    shiny::selectInput(
      ns('cubication'), 'Cubication shape', choices = cubication_choices,
      multiple = TRUE
    ),
    shiny::selectInput(
      ns('specialparam'), 'Special parameter',
      choices = specialparam_choices, multiple = TRUE
    )
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param allometries_table table with allometries info from allometr_db
#' @param variables_thesaurus table with variables info from allometr_db
#' @param cubication_thesaurus table with cubication info from allometr_db
#'
#' @export
mod_data <- function(
  input, output, session,
  allometries_table, variables_thesaurus, cubication_thesaurus
) {

  # observer to show the ui for spatial values
  shiny::observe({
    spatial <- input$spatial

    if (is.null(spatial) || spatial == '') {
      shinyjs::hide('spatial_values')
    } else {
      shinyjs::show('spatial_values')
    }
  })

  output$spatial_values <- shiny::renderUI({

    # ns
    ns <- session$ns

    # spatial level
    spatial_vals <- input$spatial

    # choices
    spatial_values_choices <- allometries_table %>%
      dplyr::filter(spatial_level %in% spatial_vals) %>%
      dplyr::pull(spatial_name) %>% unique() %>% sort()

    # UI
    shiny::tagList(
      shiny::selectInput(
        ns('spatial_values_input'), 'Select the spatial ambit values',
        choices = spatial_values_choices, multiple = TRUE
      )
    )
  })

  # observer to show the ui for functional group values
  shiny::observeEvent(
    eventExpr = input$functgroup,
    handlerExpr = {
      if (is.null(input$functgroup) || input$functgroup == '') {
        shinyjs::hide('functgroup_values_div')
      } else {
        shinyjs::show('functgroup_values_div')
      }
    }
  )

  output$functgroup_values <- shiny::renderUI({

    # ns
    ns <- session$ns

    # functgroup level
    functgroup_vals <- input$functgroup

    # choices
    functgroup_values_choices <- allometries_table %>%
      dplyr::filter(functional_group_level %in% functgroup_vals) %>%
      dplyr::pull(functional_group_name) %>% unique() %>% sort()

    # UI
    shiny::tagList(
      shiny::selectInput(
        ns('functgroup_values_input'), 'Select the functional group values',
        choices = functgroup_values_choices, multiple = TRUE
      )
    )
  })

  # gather the inputs
  data_inputs <- shiny::reactiveValues()
  shiny::observe({
    # inputs per se
    data_inputs$allolvl <- input$allolvl
    data_inputs$spatial <- input$spatial
    data_inputs$spatial_values_input <- input$spatial_values_input
    data_inputs$functgroup <- input$functgroup
    data_inputs$functgroup_values_input <- input$functgroup_values_input
    data_inputs$depvar <- input$depvar
    data_inputs$indepvars <- input$indepvars
    data_inputs$cubication <- input$cubication
    data_inputs$specialparam <- input$specialparam
  })

  # reactive to create the filtering expression
  filtering_expr <- shiny::reactive({

    # filter exprs
    rlang::quos(
      allometry_level %in% !! input$allolvl,
      spatial_level %in% !! input$spatial,
      spatial_name %in% !! input$spatial_values_input,
      functional_group_level %in% !! input$functgroup,
      functional_group_name %in% !! input$functgroup_values_input,
      dependent_var %in% !! input$depvar,
      independent_var_1 %in% !! input$indepvars ||
        independent_var_2 %in% !! input$indepvars ||
        independent_var_3 %in% !! input$indepvars,
      cubication_shape %in% !! input$cubication,
      special_parameter %in% !! input$specialparam
    ) %>%
      # removing null or empty inputs
      magrittr::extract(!purrr::map_lgl(list(
        input$allolvl,
        input$spatial,
        input$spatial_values_input,
        input$functgroup,
        input$functgroup_values_input,
        input$depvar,
        input$indepvars,
        input$cubication,
        input$specialparam
      ), is.null))

  })

  # gather the inputs
  data_inputs <- shiny::reactiveValues()
  shiny::observe({
    # inputs per se
    data_inputs$allolvl <- input$allolvl
    data_inputs$spatial <- input$spatial
    data_inputs$spatial_values_input <- input$spatial_values_input
    data_inputs$functgroup <- input$functgroup
    data_inputs$functgroup_values_input <- input$functgroup_values_input
    data_inputs$depvar <- input$depvar
    data_inputs$indepvars <- input$indepvars
    data_inputs$cubication <- input$cubication
    data_inputs$specialparam <- input$specialparam
    # filtering expr
    data_inputs$filtering_expr <- filtering_expr()
  })

  return(data_inputs)
}