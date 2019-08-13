#' Get allometries descriptions as a list
#'
#' This function returns the allometries descriptions
#'
#' This function can return the description of the alometries given in the id argument
#' or can filter the general allometries table with the expressions given in the ...
#' argument
#'
#' @param ... Expresions resolving to a logical value to filter the allometries. Only
#'   evaluated if \code{id} is NULL.
#' @param id Character vector with the allometry/ies id
#'
#' @examples
#'
#' # by id
#' foo <- allom_description(id = "GC_2589")
#' foo$GC_2589$dependent_var
#' foo$GC_2589$param_a
#'
#' # filtering
#' ht_dn_allometries <- allom_description(dependent_var %in% c("GC", "Dn"))
#' ht_dn_allometries$GC_2589$dependent_var
#' ht_dn_allometries$GC_2589$param_a
#'
#' @return a list with the allometries and their
#'
#' @export
allom_description <- function(..., id = NULL) {
  dots_expressions <- rlang::quos(...)

  if (is.null(id)) {
    res <- allometrApp::allometries_table %>%
      dplyr::filter(!!! dots_expressions) %>%
      split(.$allometry_id) %>%
      purrr::map(~ rlang::as_list(.x))
  } else {
    res <- allometrApp::allometries_table %>%
      dplyr::filter(allometry_id %in% id) %>%
      split(.$allometry_id) %>%
      purrr::map(~ rlang::as_list(.x))
  }

  if (length(res) < 1) {
    warning("No allometries were found. Returning an empty list")
  }
  return(res)
}

eq_formatter <- function(eq) {

  eq_res <- eq

  # · to *
  eq_res <- stringr::str_replace_all(eq_res, '·', '*')
  # ² to ^2
  eq_res <- stringr::str_replace_all(eq_res, '²', '^2')
  eq_res <- stringr::str_replace_all(eq_res, '³', '^3')
  # params
  eq_res <- stringr::str_replace(eq_res, '\\ba\\b', 'param_a')
  eq_res <- stringr::str_replace(eq_res, '\\bb\\b', 'param_b')
  eq_res <- stringr::str_replace(eq_res, '\\bc\\b', 'param_c')
  eq_res <- stringr::str_replace(eq_res, '\\bd\\b', 'param_d')

  return(eq_res)

}

# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

# translate app function
translate_app <- function(id, lang, db) {

  thesaurus <- dplyr::tbl(db, tolower('THESAURUS_APP')) %>%
    dplyr::collect()

  id %>%
    purrr::map_chr(
      ~ thesaurus %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )

  # dplyr::tbl(db, tolower('THESAURUS_APP')) %>%
  #   dplyr::filter(text_id %in% id) %>%
  #   dplyr::arrange(text_id) %>%
  #   dplyr::pull(!! rlang::sym(glue::glue("translation_{lang}")))
}

# variables description table
variables_description <- function(lang, db) {
  dplyr::tbl(db, tolower('THESAURUS_VARIABLES')) %>%
    dplyr::select(var_id, dplyr::contains(glue::glue('translation_{lang}')), var_units) %>%
    dplyr::collect()
}

#### garbage ####
# selectizeGroupUI_custom <- function(
#   id, params, label = NULL, btn_label = "Reset filters", inline = TRUE
# ) {
#   ns <- shiny::NS(id)
#   if (inline) {
#     selectizeGroupTag <- shiny::tagList(shiny::tags$b(label), shiny::tags$div(
#       class = "btn-group-justified selectize-group",
#       role = "group", `data-toggle` = "buttons", lapply(
#         X = seq_along(params),
#         FUN = function(x) {
#           input <- params[[x]]
#           tagSelect <- shiny::tags$div(
#             class = "btn-group",
#             shiny::selectizeInput(
#               inputId = ns(input$inputId),
#               label = input$title, choices = input$choices,
#               selected = input$selected, multiple = TRUE,
#               width = "100%", options = list(
#                 placeholder = input$placeholder,
#                 plugins = list("remove_button"), onInitialize = I("function() { this.setValue(\"\"); }")
#               )
#             )
#           )
#           return(tagSelect)
#         }
#       )
#     ), shiny::actionLink(
#       inputId = ns("reset_all"), label = btn_label,
#       icon = shiny::icon("remove"), style = "float: right;"
#     ))
#   }
#   else {
#     selectizeGroupTag <- shiny::tagList(shiny::tags$b(label), lapply(
#       X = seq_along(params),
#       FUN = function(x) {
#         input <- params[[x]]
#         tagSelect <- shiny::selectizeInput(
#           inputId = ns(input$inputId),
#           label = input$title, choices = input$choices,
#           selected = input$selected, multiple = TRUE,
#           width = "100%", options = list(
#             placeholder = input$placeholder,
#             plugins = list("remove_button"), onInitialize = I("function() { this.setValue(\"\"); }")
#           )
#         )
#         return(tagSelect)
#       }
#     ), shiny::actionLink(
#       inputId = ns("reset_all"), label = btn_label,
#       icon = shiny::icon("remove"), style = "float: right;"
#     ))
#   }
#   shiny::tagList(shiny::singleton(shiny::tagList(
#     shiny::tags$link(
#       rel = "stylesheet",
#       type = "text/css", href = "shinyWidgets/modules/styles-modules.css"
#     ),
#     toggleDisplayUi_custom()
#   )), selectizeGroupTag)
# }
#
# toggleDisplayUi_custom <- function() {
#   htmltools::tags$script(paste("Shiny.addCustomMessageHandler('toggleDisplay',",
#                                "function(data) {", "$('#' + data.id).css('display', data.display);",
#                                "});", sep = "\n"))
# }
#
# selectizeGroupServer_custom <- function(input, output, session, data, vars, lang, db) {
#   data <- as.data.frame(data)
#   ns <- session$ns
#   toggleDisplayServer_custom(
#     session = session, id = ns("reset_all"),
#     display = "none"
#   )
#   lapply(X = vars, FUN = function(x) {
#
#     if (x %in% c(
#       'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
#       'allometry_level', 'spatial_level', 'functional_group_level'
#     )) {
#       vals <- sort(unique(data[[x]])) %>% {
#         magrittr::set_names(., translate_app(., 'cat', db))
#       }
#     } else {
#       vals <- sort(unique(data[[x]]))
#     }
#
#     shiny::updateSelectizeInput(
#       session = session, inputId = x,
#       choices = vals, server = TRUE
#     )
#   })
#   shiny::observeEvent(input$reset_all, {
#     lapply(X = vars, FUN = function(x) {
#
#       if (x %in% c(
#         'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
#         'allometry_level', 'spatial_level', 'functional_group_level'
#       )) {
#         vals <- sort(unique(data[[x]])) %>% {
#           magrittr::set_names(., translate_app(., lang(), db))
#         }
#       } else {
#         vals <- sort(unique(data[[x]]))
#       }
#
#       shiny::updateSelectizeInput(
#         session = session, inputId = x,
#         choices = vals, server = TRUE
#       )
#     })
#   })
#   shiny::observeEvent(lang(), {
#     lapply(X = vars, FUN = function(x) {
#
#       if (x %in% c(
#         'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
#         'allometry_level', 'spatial_level', 'functional_group_level'
#       )) {
#         vals <- sort(unique(data[[x]])) %>% {
#           magrittr::set_names(., translate_app(., lang(), db))
#         }
#       } else {
#         vals <- sort(unique(data[[x]]))
#       }
#
#       shiny::updateSelectizeInput(
#         session = session, inputId = x,
#         choices = vals, server = TRUE
#       )
#     })
#   })
#   lapply(X = vars, FUN = function(x) {
#     ovars <- vars[vars != x]
#     shiny::observeEvent(input[[x]], {
#
#       indicator <- lapply(X = vars, FUN = function(x) {
#         data[[x]] %inT% input[[x]]
#       })
#       indicator <- Reduce(f = `&`, x = indicator)
#       data <- data[indicator, ]
#       if (all(indicator)) {
#         toggleDisplayServer_custom(
#           session = session, id = ns("reset_all"),
#           display = "none"
#         )
#       }
#       else {
#         toggleDisplayServer_custom(
#           session = session, id = ns("reset_all"),
#           display = "block"
#         )
#       }
#       for (i in ovars) {
#         if (is.null(input[[i]])) {
#
#           if (i %in% c(
#             'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
#             'allometry_level', 'spatial_level', 'functional_group_level'
#           )) {
#             vals <- sort(unique(data[[i]])) %>% {
#               magrittr::set_names(., translate_app(., lang(), db))
#             }
#           } else {
#             vals <- sort(unique(data[[i]]))
#           }
#
#           shiny::updateSelectizeInput(
#             session = session, inputId = i,
#             choices = vals, server = TRUE
#           )
#         }
#       }
#       if (is.null(input[[x]])) {
#
#         if (x %in% c(
#           'dependent_var', 'independent_var_1', 'independent_var_2', #'independent_var_3',
#           'allometry_level', 'spatial_level', 'functional_group_level'
#         )) {
#           vals <- sort(unique(data[[x]])) %>% {
#             magrittr::set_names(., translate_app(., lang(), db))
#           }
#         } else {
#           vals <- sort(unique(data[[x]]))
#         }
#
#         shiny::updateSelectizeInput(
#           session = session, inputId = x,
#           choices = vals, server = TRUE
#         )
#       }
#     }, ignoreNULL = FALSE, ignoreInit = TRUE)
#   })
#   return(shiny::reactive({
#     indicator <- lapply(X = vars, FUN = function(x) {
#       data[[x]] %inT% input[[x]]
#     })
#     indicator <- Reduce(f = `&`, x = indicator)
#     data <- data[indicator, ]
#     return(data)
#   }))
# }
#
# toggleDisplayServer_custom <- function(session, id, display = c("none", "block", "inline-block"))
# {
#   display <- match.arg(display)
#   session$sendCustomMessage(type = "toggleDisplay", message = list(id = id,
#                                                                    display = display))
# }
#
# `%inT%` <- function(x, table) {
#   if (!is.null(table) && ! "" %in% table) {
#     x %in% table
#   } else {
#     rep_len(TRUE, length(x))
#   }
# }