#' Rename nutrients columns
#'
#' Rename nutrients column with the format "Nutrient_mu" (from GitHub raw models data)
#' with "nutrient".
#' @param df Dataframe containing nutrients columns to rename.
#' @param hyphen Whether to use hyphen in renaming "omega_3" and "vitamin_a".
#'
#' @return A dataframe with renamed columns
#' @export
#'
#' @examples
#' \dontrun{
#' rename_nutrients_mu(nutrients_table)
#' }
rename_nutrients_mu <- function(df = NULL, hyphen = FALSE) {
  if (isFALSE(hyphen)) {
    df %>%
      dplyr::rename_with(~ tolower(.), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("_mu$", "", .), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("omega_3", "omega3", .), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("vitamin_a", "vitaminA", .), dplyr::everything())
  } else {
    df %>%
      dplyr::rename_with(~ tolower(.), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("_mu$", "", .), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("omega_3", "omega-3", .), dplyr::everything()) %>%
      dplyr::rename_with(~ gsub("vitamin_a", "vitamin-A", .), dplyr::everything())
  }
}

#' Generate Modeling Datasets
#'
#' Prepares and returns datasets for use with an ML model. This function generates
#' four datasets: two focus exclusively on gill net data, and the other two cover all other gear types.
#' These datasets are specific to Atauro and other municipalities.
#'
#' The decision to divide the dataset into four subsets is driven by the
#' imbalance present in the raw survey data from Peskas. A significant portion of the data (>40%)
#' relates to Atauro, and the most common gear type is the gill net.
#'
#' By splitting the data into these subsets, we aim to reduce the bias caused by
#' the overrepresentation of Atauro and gill net observations. Additionally, analyzing
#' only the gill net data separately allows for an evaluation of the impact of mesh size
#' on predicting nutrient profiles.
#'
#' @return A list containing the four datasets. The list also includes kmeans profiles plots.
#' @export
#'
#' @examples
#' \dontrun{
#' data_list <- get_model_data()
#'
#' structured_results <-
#'   purrr::imap(
#'     data_list, ~ run_xgmodel(
#'       dataframe = .x$dataframe, step_other = .x$step_other
#'     )
#'   ) %>%
#'   setNames(paste0("model_", names(.)))
#' }
#'
get_model_data <- function() {
  df <-
    timor.nutrients::kobo_trips %>%
    dplyr::filter(gear_type == "gill net") %>%
    dplyr::ungroup() %>%
    dplyr::select(-Selenium_mu) %>%
    rename_nutrients_mu() %>%
    tidyr::pivot_longer(c(zinc:vitaminA), names_to = "nutrient", values_to = "kg") %>%
    dplyr::left_join(RDI_tab, by = "nutrient") %>%
    dplyr::mutate(
      nutrients_kg_per_kg = kg / weight, # standardize nutrients for 1 kg of catch
      nutrients_g_per_kg = nutrients_kg_per_kg * 1000, # convert stand nutrients in grams
      people_rni_kg = nutrients_g_per_kg / conv_factor
    ) %>%
    dplyr::select(landing_id, landing_date, vessel_type, habitat, gear_type, mesh_size, nutrient, people_rni_kg) %>%
    tidyr::pivot_wider(names_from = "nutrient", values_from = "people_rni_kg") %>%
    dplyr::mutate(quarter = lubridate::quarter(landing_date)) %>%
    dplyr::select(landing_date, quarter, dplyr::everything()) %>%
    dplyr::group_by(landing_id, landing_date, quarter, vessel_type, habitat, gear_type, mesh_size) %>%
    dplyr::summarise(dplyr::across(is.numeric, ~ median(.x, na.rm = T))) %>%
    dplyr::ungroup() %>%
    na.omit()

  #factoextra::fviz_nbclust(df[, 8:13], kmeans, method = "wss")
  set.seed(555)
  k2 <- kmeans(df[, 8:13], centers = 5, nstart = 500)

  timor_GN_raw <-
    dplyr::tibble(
      clusters = as.character(k2$cluster),
      df
    )

  timor_GN <-
    dplyr::tibble(
      clusters = as.character(k2$cluster),
      df
    ) %>%
    dplyr::select(quarter, habitat, mesh_size, vessel_type, cluster = clusters) %>%
    dplyr::mutate(dplyr::across(.cols = c(quarter, habitat, vessel_type, cluster), ~ as.factor(.x)))

  profiles_plot_timor_GN <-
    factoextra::fviz_cluster(k2,
      data = df[, 8:13],
      geom = c("point"),
      shape = 19,
      alpha = 0.25,
      pointsize = 1.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = timor.nutrients::palettes$clusters_palette) +
    ggplot2::scale_color_manual(values = timor.nutrients::palettes$clusters_palette) +
    ggplot2::labs(title = "") +
    ggplot2::theme(legend.position = "bottom")

  df <-
    timor.nutrients::kobo_trips %>%
    dplyr::filter(!gear_type == "gill net") %>%
    dplyr::ungroup() %>%
    dplyr::select(-Selenium_mu) %>%
    rename_nutrients_mu() %>%
    tidyr::pivot_longer(c(zinc:vitaminA), names_to = "nutrient", values_to = "kg") %>%
    dplyr::left_join(RDI_tab, by = "nutrient") %>%
    dplyr::mutate(
      nutrients_kg_per_kg = kg / weight, # standardize nutrients for 1 kg of catch
      nutrients_g_per_kg = nutrients_kg_per_kg * 1000, # convert stand nutrients in grams
      people_rni_kg = nutrients_g_per_kg / conv_factor
    ) %>%
    dplyr::select(landing_id, landing_date, vessel_type, habitat, gear_type, nutrient, people_rni_kg) %>%
    tidyr::pivot_wider(names_from = "nutrient", values_from = "people_rni_kg") %>%
    dplyr::mutate(quarter = lubridate::quarter(landing_date)) %>%
    dplyr::select(landing_date, quarter, dplyr::everything()) %>%
    dplyr::group_by(landing_id, landing_date, quarter, vessel_type, habitat, gear_type) %>%
    dplyr::summarise(dplyr::across(is.numeric, ~ median(.x, na.rm = T))) %>%
    dplyr::ungroup() %>%
    na.omit()

  #factoextra::fviz_nbclust(df[, 7:12], kmeans, method = "wss")
  set.seed(555)
  k2 <- kmeans(df[, 7:12], centers = 5, nstart = 500)

  timor_AG_raw <-
    dplyr::tibble(
      clusters = as.character(k2$cluster),
      df
    )

  timor_AG <-
    dplyr::tibble(
      clusters = as.character(k2$cluster),
      df
    ) %>%
    dplyr::mutate(habitat_gear = paste(habitat, gear_type, sep = "_")) %>%
    dplyr::select(quarter, habitat_gear, habitat, gear_type, vessel_type, cluster = clusters) %>%
    dplyr::mutate(dplyr::across(.cols = c(quarter, habitat_gear, habitat, gear_type, vessel_type, cluster), ~ as.factor(.x)))

  profiles_plot_timor_AG <-
    factoextra::fviz_cluster(k2,
      data = df[, 7:12],
      geom = c("point"),
      shape = 19,
      alpha = 0.25,
      pointsize = 1.5
    ) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = timor.nutrients::palettes$clusters_palette) +
    ggplot2::scale_color_manual(values = timor.nutrients::palettes$clusters_palette) +
    ggplot2::labs(title = "") +
    ggplot2::theme(legend.position = "bottom")

  # Create a named list of dataframes and parameters

  data_list_raw <- list(
    timor_AG_raw = timor_AG_raw,
    timor_GN_raw = timor_GN_raw
  )

  data_list_processed <- list(
    timor_AG = list(dataframe = timor_AG, step_other = c("habitat_gear", "habitat", "gear_type")),
    timor_GN = list(dataframe = timor_GN, step_other = "habitat")
  )

  profiles_kmeans <- list(
    kmeans_timor_GN = profiles_plot_timor_GN,
    kmeans_timor_AG = profiles_plot_timor_AG
  )

  list(
    data_raw = data_list_raw,
    kmeans_plots = profiles_kmeans,
    data_processed = data_list_processed
  )
}


#' Run PERMANOVA on Clustered Data
#'
#' This function performs a Permutational Multivariate Analysis of Variance (PERMANOVA)
#' on a given dataset. It first selects a range of variables  (from zinc to vitaminA)
#' and clusters, then calculates the Euclidean distance matrix,
#' and finally runs PERMANOVA using the `vegan::adonis2` function.
#'
#' @param x A dataframe containing the variables of interest and a 'clusters' group column.
#' @param permutations The number of permutations to be used in the PERMANOVA analysis.
#' @param parallel Optional; a parallel backend to be used with the PERMANOVA analysis. If NULL, parallel processing is not used.
#' @return A PERMANOVA result in tidy object from `broom::tidy`.
#' @importFrom dplyr select
#' @importFrom vegan vegdist adonis2
#' @examples
#' \dontrun{
#' # Example usage:
#' # run_permanova_clusters(your_data, 999, parallel = 2)
#' }
#' @export
#'
run_permanova_clusters <- function(x, permutations = NULL, parallel = NULL) {
  anov_dat <-
    x %>%
    dplyr::select(clusters, zinc:vitaminA)

  dist_matrix <- vegan::vegdist(anov_dat[, -1], method = "euclidean")
  # Perform PERMANOVA using the adonis function
  permanova_results <- vegan::adonis2(dist_matrix ~ clusters,
    data = anov_dat,
    parallel = parallel,
    permutations = permutations,
    save.memory = TRUE
  )
  broom::tidy(permanova_results)
}
