#' function to launch the nfi app
#'
#' @param file character indicating the flexdashboard Rmd file to load
#' @param shiny_args list with the shiny arguments for runApp
#' @param render_params list of named parameters for the Rmd file
#'
#' @export
allometr_app <- function(
  file,
  shiny_args = list(host = '0.0.0.0', port = 3838),
  render_params =  list(
    appdb_user = 'guest',
    appdb_password = 'guest',
    appdb_dbname = 'allometr_db',
    appdb_host = 'localhost'
  )
) {

  # wrapper for rmarkdown::run with the corresponding arguments
  rmarkdown::run(
    file = file, shiny_args = shiny_args, render_args = list(params = render_params)
  )
}
