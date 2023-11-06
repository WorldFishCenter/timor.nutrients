#' Rename nutrients columns
#'
#' Rename nutrients column with the format "Nutrient_mu" (from GitHub raw models data)
#' with "nutrient".
#' @param df Dataframe containing nutrients columns to rename.
#'
#' @return A dataframe with renamed columns
#' @export
#'
#' @examples
#' \dontrun{
#' rename_nutrients_mu(nutrients_table)
#' }
rename_nutrients_mu <- function(df = NULL) {
  df %>%
    dplyr::rename_with(~ tolower(.), dplyr::everything()) %>%
    dplyr::rename_with(~ gsub("_mu$", "", .), dplyr::everything()) %>%
    dplyr::rename_with(~ gsub("omega_3", "omega3", .), dplyr::everything()) %>%
    dplyr::rename_with(~ gsub("vitamin_a", "vitaminA", .), dplyr::everything())
}
