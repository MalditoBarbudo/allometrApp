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
    res <- allometries_table %>%
      dplyr::filter(!!! dots_expressions) %>%
      split(.$allometry_id) %>%
      purrr::map(~ rlang::as_list(.x))
  } else {
    res <- allometries_table %>%
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
  # params
  eq_res <- stringr::str_replace(eq_res, '\\ba\\b', 'param_a')
  eq_res <- stringr::str_replace(eq_res, '\\bb\\b', 'param_b')
  eq_res <- stringr::str_replace(eq_res, '\\bc\\b', 'param_c')

  return(eq_res)

}
