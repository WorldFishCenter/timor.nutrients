#' Read configuration file
#'
#' @return a list of package configuration attributes
#' @export
#'
read_config <- function() {
  pars <- config::get(
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    #file = system.file("config.yml", package = "Timor.nutrients")
    file = system.file("config.yml", package = "Timor.nutrients")
  )
  pars
}
