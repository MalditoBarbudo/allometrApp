#' Calculating new variables based on the allometries formula
#'
#' Given the allometry id, this function creates a new variable with the desired allometry
#'
#' This is a tidy-friendly funcion, meanining it expects a dataframe and will return
#' a dataframe with the new variable calculated.
#'
#' @param data data frame with at least, the independent variables needed for calculating
#'   the allometry
#' @param ... variable names for the independent variables
#' @param allometry_id character with the unique allometry identifier
#' @param name Optional, the name of the variable created. If not provided, the name will
#'   be the allometry_id
#'
#' @examples
#'
#' data_example <- data.frame(Dn = rnorm(10, 12, 2))
#'
#' data_example %>%
#'   allom_calculate(
#'     Dn = Dn,
#'     allometry_id = 'BAT_1',
#'     name = 'BAT'
#'   )
#'
#' @export
allom_calculate <- function(
  data, ..., allometry_id, name = allometry_id
) {

  # variables
  dots_vars <- rlang::enquos(..., .named = TRUE)
  # allometry description
  allo_desc <- allom_description(id = allometry_id)

  # equation modification to include it as a mutate argument
  eq_for_mutate <- allo_desc[[allometry_id]][['equation']] %>%
    stringr::str_split(pattern = ' = ', n = 2) %>%
    magrittr::extract2(1) %>%
    magrittr::extract(2) %>% {
      eq <- .
      for (var in names(dots_vars)) {
        eq <- stringr::str_replace_all(
          eq, pattern = var,
          replacement = dots_vars[[var]] %>% rlang::as_name()
        )
      }
      eq
    } %>%
    eq_formatter()

  # parameters from allometry (needed in equation)
  param_a <- allo_desc[[allometry_id]][['param_a']]
  param_b <- allo_desc[[allometry_id]][['param_b']]
  param_c <- allo_desc[[allometry_id]][['param_c']]

  data %>%
    dplyr::mutate(
      !!name := !! rlang::parse_expr(eq_for_mutate)
    )
}