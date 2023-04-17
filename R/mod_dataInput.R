#' @title mod_dataInput and mod_data
#'
#' @description A shiny module to create and populate the data inputs, linked between them.
#'   It is based on shinyWidgets::selectizeGroupUI module, but adapted to our case
#'
#' @param id shiny id
#' @param params list with params
#' @param label label
#' @param btn_label button label
#' @param inline logical
#'
#' @export
mod_dataInput <- function(
  id, params, label = NULL, btn_label = "Restableix els filtres", inline = TRUE
) {

  # ns
  ns <- shiny::NS(id)

  if (isTRUE(inline)) {
    # inline
    selectizeGroupTag <- shiny::tagList(
      shiny::b(label),
      shiny::div(
        # attrbs
        class = "btn-group-justified selectize-group",
        role = 'group',
        `data-toggle` = 'buttons',
        # proper ui
        purrr::map(
          seq_along(params),
          function(x) {
            input <- params[[x]]
            tagSelect <- shiny::div(
              class = 'btn-group',
              shiny::selectizeInput(
                inputId = ns(input$inputId),
                label = input$title, choices = input$choices,
                selected = input$selected, multiple = TRUE, width = "100%",
                options = list(
                  placeholder = input$placeholder,
                  plugins = list("remove_button"),
                  onInitialize = I("function() { this.setValue(\"\"); }")
                )
              )
            )
            return(tagSelect)
          }
        )
      ),
      shiny::actionLink(
        ns("reset_all"), label = btn_label, icon = shiny::icon("remove"),
        style = "float; right;"
      )
    )
  } else {
    # No inline
    selectizeGroupTag <- shiny::tagList(
      shiny::tags$b(label),
      purrr::map(
        seq_along(params),
        function(x) {
          input <- params[[x]]
          tagSelect <- shiny::selectizeInput(
            ns(input$inputId), label = input$title, choices = input$choices,
            selected = input$selected, multiple = TRUE, width = "100%",
            options = list(
              placeholder = input$placeholder, plugins = list("remove_button"),
              onInitialize = I("function() { this.setValue(\"\"); }")
            )
          )
          return(tagSelect)
        }
      ),
      shiny::actionLink(
        ns("reset_all"), label = btn_label, icon = shiny::icon("times"),
        style = "float; right;"
      )
    )
  }

  shiny::tagList(
    shiny::singleton(
      shiny::tagList(
        shiny::tags$link(
          rel = "stylesheet", type = "text/css",
          href = "shinyWidgets/modules/style-modules.css"
        ),
        toggleDisplayUi_custom()
      )
    ),
    selectizeGroupTag
  )
}

#' mod_data server function
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @param data data
#' @param vars vars
#' @param lang lang value
#' @param db database
#'
#' @export
mod_data <- function(
  input, output, session,
  data, vars, lang, db
) {

  data <- as.data.frame(data)
  ns <- session$ns
  toggleDisplayServer_custom(session = session, id = ns('reset_all'), display = "none")

  # initial update
  purrr::walk(vars, function(x) {
    names_to_translate <- sort(unique(data[[x]]))
    vals <- names_to_translate |>
      purrr::set_names(translate_app(names_to_translate, 'cat', db))
    shiny::updateSelectizeInput(
      session = session, inputId = x,
      choices = vals, server = TRUE
    )
  })

  # observe for reset all event
  shiny::observeEvent(
    eventExpr = input$reset_all,
    handlerExpr = {
      # reset update
      purrr::walk(vars, function(x) {
        names_to_translate <- sort(unique(data[[x]]))
        vals <- names_to_translate |>
          purrr::set_names(translate_app(names_to_translate, lang(), db))
        shiny::updateSelectizeInput(
          session = session, inputId = x,
          choices = vals, server = TRUE
        )
      })
    }
  )

  # observe for lang change event
  shiny::observeEvent(
    eventExpr = lang(),
    handlerExpr = {
      # reset update
      purrr::walk(vars, function(x) {
        names_to_translate <- sort(unique(data[[x]]))
        vals <- names_to_translate |>
          purrr::set_names(translate_app(names_to_translate, lang(), db))
        shiny::updateSelectizeInput(
          session = session, inputId = x,
          choices = vals, server = TRUE
        )
      })
    }
  )

  # check for each input to update based on the filters made in other inputs
  purrr::walk(
    vars,
    function(x) {
      # outer vars
      ovars <- vars[vars != x]
      # observe event
      shiny::observeEvent(
        ignoreNULL = FALSE, ignoreInit = TRUE,
        eventExpr = input[[x]],
        handlerExpr = {

          # browser()

          indicator <- purrr::map(vars, function(x) {
            data[[x]] %inT% input[[x]]
          }) |>
            purrr::reduce(`&`)

          data <- data[indicator, ]

          if (all(indicator)) {
            toggleDisplayServer_custom(
              session = session, id = ns("reset_all"),
              display = "none"
            )
          } else {
            toggleDisplayServer_custom(
              session = session, id = ns("reset_all"),
              display = "block"
            )
          }

          # now we iterate over outer vars to update the values with the modified data
          purrr::walk(
            ovars,
            function(x) {
              if (is.null(input[[x]])) {
                names_to_translate <- sort(unique(data[[x]]))
                vals <- names_to_translate |>
                  purrr::set_names(translate_app(names_to_translate, lang(), db))
                shiny::updateSelectizeInput(
                  session = session, inputId = x,
                  choices = vals, server = TRUE
                )
              }
            }
          )

          # update the proper input we are iterating
          if (is.null(input[[x]])) {
            names_to_translate <- sort(unique(data[[x]]))
            vals <- names_to_translate |>
              purrr::set_names(translate_app(names_to_translate, lang(), db))
            shiny::updateSelectizeInput(
              session = session, inputId = x,
              choices = vals, server = TRUE
            )
          }
        }
      )
    }
  )

  # finally a reactive to return the data after all the multiple filters
  data_res <- shiny::reactive({

    indicator <- purrr::map(vars, function(x) {
      data[[x]] %inT% input[[x]]
    }) |>
      purrr::reduce(`&`)

    data[indicator, ]
  })

  return(data_res)
}

toggleDisplayUi_custom <- function() {
  shiny::tags$script(paste(
    "Shiny.addCustomMessageHandler('toggleDisplay',",
    "function(data) {", "$('#' + data.id).css('display', data.display);",
    "});", sep = "\n"
  ))
}

toggleDisplayServer_custom <- function(session, id, display = c("none", "block", "inline-block")) {
  display <- match.arg(display)
  session$sendCustomMessage(
    type = "toggleDisplay", message = list(id = id, display = display)
  )
}

`%inT%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}
