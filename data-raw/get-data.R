library(magrittr)

pars <- read_config()

palettes <- list(
  nutrients_palette = pars$nutrients$pal_nutrients,
  clusters_palette = pars$nutrients$pal_clusters
)

usethis::use_data(palettes, overwrite = TRUE)

RDI_tab <-
  tidyr::tibble(
    nutrient = c("selenium", "zinc", "protein", "omega3", "calcium", "iron", "vitaminA"),
    conv_factor = c(0.000026, 0.0049, 46, 2.939, 1, 0.0294, 0.0005)
  )

timor_population <-
  readr::read_csv(system.file("timor_census_2022.csv", package = "timor.nutrients")) %>%
  dplyr::filter(gender == "Female") %>%
  dplyr::group_by(region) %>%
  dplyr::filter(age > 14 & age < 50) %>%
  dplyr::summarise(population = sum(count, na.rm = T)) %>%
  dplyr::add_row(region = "All", population = sum(.$population)) %>%
  dplyr::ungroup()

catch_groups <-
  readr::read_csv(system.file("catch_groups.csv", package = "timor.nutrients")) %>%
  dplyr::select(interagency_code, family, catch_name)

nutrients_table <-
  get_nutrients_table(pars, convert = TRUE, summarise = TRUE) %>%
  dplyr::rename(grouped_taxa = interagency_code) %>%
  dplyr::mutate_at(
    dplyr::vars(Selenium_mu:Vitamin_A_mu),
    ~ tidyr::replace_na(., median(., na.rm = TRUE))
  )

# region_stats <-
#  get_models(pars) %>%
#  dplyr::left_join(nutrients_table, by = "grouped_taxa") %>%
#  dplyr::mutate(dplyr::across(c(Selenium_mu:Vitamin_A_mu), ~ .x * catch)) %>%
#  rename_nutrients_mu()

region_data <- readr::read_rds(system.file("estimations_kg_12_2023_v2.rds", package = "timor.nutrients"))
region_stats <-
  region_data %>%
  purrr::map(~ purrr::keep(.x, stringr::str_detect(
    names(.x), stringr::fixed("taxa")
  ))) %>%
  purrr::list_flatten(name_spec = "{outer}") %>%
  dplyr::bind_rows(.id = "region") %>%
  dplyr::rename(date_bin_start = .data$landing_period) %>%
  dplyr::select(c(.data$region, .data$date_bin_start, .data$grouped_taxa, .data$catch)) %>%
  dplyr::left_join(nutrients_table, by = "grouped_taxa") %>%
  dplyr::mutate(dplyr::across(c(Selenium_mu:Vitamin_A_mu), ~ .x * catch)) %>%
  rename_nutrients_mu() %>%
  dplyr::filter(date_bin_start < "2024-01-01") %>%
  rename_nutrients_mu()

#  dplyr::left_join(nutrients_table, by = "grouped_taxa") %>%
#  dplyr::transmute(
#    region = .data$region,
#    date_bin_start = .data$date_bin_start,
#    grouped_taxa = .data$grouped_taxa,
#    selenium = (.data$Selenium_mu * (.data$catch * 1000)) / 1000,
#    zinc = (.data$Zinc_mu * (.data$catch * 1000)) / 1000,
#    catch = .data$catch,
#    protein = (.data$Protein_mu * (.data$catch * 1000)) / 1000,
#    omega3 = (.data$Omega_3_mu * (.data$catch * 1000)) / 1000,
#    calcium = (.data$Calcium_mu * (.data$catch * 1000) / 1000),
#    iron = (.data$Iron_mu * (.data$catch * 1000)) / 1000,
#    vitaminA = (.data$Vitamin_A_mu * (.data$catch * 1000)) / 1000
#  )

trips <- get_merged_trips(pars) %>% dplyr::filter(!is.na(landing_id))

kobo_trips <-
  trips %>%
  dplyr::mutate(
    landing_period = lubridate::floor_date(landing_date, unit = "month"),
    landing_id = as.character(landing_id),
    n_fishers = fisher_number_man + fisher_number_woman + fisher_number_child
  ) %>%
  dplyr::filter(landing_period >= "2018-01-01" & landing_period <= "2023-12-31") %>%
  tidyr::unnest(.data$landing_catch) %>%
  tidyr::unnest(.data$length_frequency) %>%
  dplyr::filter(!is.na(.data$weight)) %>%
  dplyr::group_by(.data$landing_id, .data$landing_period) %>%
  dplyr::summarise(
    landing_id = dplyr::first(landing_id),
    reporting_region = dplyr::first(reporting_region),
    landing_date = dplyr::first(landing_date),
    trip_duration = dplyr::first(trip_duration),
    n_fishers = dplyr::first(n_fishers),
    habitat = dplyr::first(habitat),
    gear_type = dplyr::first(gear_type),
    mesh_size = dplyr::first(mesh_size),
    vessel_type = dplyr::first(vessel_type),
    landing_value = dplyr::first(landing_value),
    dplyr::across(
      c(.data$weight:.data$Vitamin_A_mu),
      ~ sum(.x) / 1000
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    habitat = ifelse(habitat == "Traditional FAD", "FAD", habitat),
    reporting_region = dplyr::case_when(
      .data$reporting_region %in% c("Bobonaro", "Liquica", "Dili", "Baucau", "Oecusse", "Manatuto", "Lautem") ~ "North Coast",
      .data$reporting_region == "Atauro" ~ "Atauro",
      TRUE ~ "South Coast"
    )
  )

catch_data <-
  trips %>%
  dplyr::filter(landing_id %in% kobo_trips$landing_id) %>%
  dplyr::select(landing_id, landing_value, landing_catch)

usethis::use_data(RDI_tab, overwrite = TRUE)
usethis::use_data(catch_groups, overwrite = TRUE)
usethis::use_data(timor_population, overwrite = TRUE)
usethis::use_data(nutrients_table, overwrite = TRUE)
usethis::use_data(region_stats, overwrite = TRUE)
usethis::use_data(kobo_trips, overwrite = TRUE)
usethis::use_data(catch_data, overwrite = TRUE)
devtools::document()
# permanova
# data_list <- get_model_data()
# set.seed(555)
# data_clusters <-
#  list(
#    atauro_AG_perm = dplyr::slice_sample(data_list$data_raw$atauro_AG_raw, prop = .5),
#    timor_GN_perm = dplyr::slice_sample(data_list$data_raw$timor_GN_raw, prop = .5),
#    atauro_GN_perm = dplyr::slice_sample(data_list$data_raw$atauro_GN_raw, prop = .5),
#    timor_AG_perm = dplyr::slice_sample(data_list$data_raw$timor_AG_raw, prop = .5)
#  )
# perm_results <- purrr::imap(data_clusters, ~ run_permanova_clusters(.x, permutations = 999, parallel = 8))
# usethis::use_data(perm_results, overwrite = T)
# devtools::document()

# Run XGBoost model
data_list <- get_model_data()$data_processed

model_outputs <-
  purrr::imap(
    data_list, ~ run_xgmodel
    (dataframe = .x$dataframe, step_other = .x$step_other, n_cores = 7)
  ) %>%
  setNames(paste0("model_", names(.)))

usethis::use_data(model_outputs, overwrite = TRUE)
devtools::document()

# Get shap values
shap_results <- purrr::map(timor.nutrients::model_outputs, run_kernelshap, parallel = TRUE, cores = 6)
usethis::use_data(shap_results, overwrite = T)
devtools::document()
