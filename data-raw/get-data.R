## code to prepare `get_data` goes here
library(magrittr)

pars <- read_config()

RDI_tab <-
  tidyr::tibble(
    nutrient = c("selenium", "zinc", "protein", "omega3", "calcium", "iron", "vitaminA"),
    conv_factor = c(0.000026, 0.0049, 46, 2.939, 1, 0.0294, 0.0005)
  )

timor_population <-
  readr::read_csv(system.file("timor_census_2022.csv", package = "timor.nutrients")) %>%
  dplyr::filter(!gender == "Total") %>%
  dplyr::group_by(region) %>%
  dplyr::filter(age > 5) %>%
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


region_stats <-
  readr::read_rds(system.file("estimations_kg_10_2023.rds", package = "timor.nutrients")) %>%
  dplyr::left_join(nutrients_table, by = "grouped_taxa") %>%
  dplyr::transmute(
    region = .data$region,
    date_bin_start = .data$date_bin_start,
    grouped_taxa = .data$grouped_taxa,
    catch = .data$catch,
    selenium = (.data$Selenium_mu * (.data$catch * 1000)) / 1000,
    zinc = (.data$Zinc_mu * (.data$catch * 1000)) / 1000,
    protein = (.data$Protein_mu * (.data$catch * 1000)) / 1000,
    omega3 = (.data$Omega_3_mu * (.data$catch * 1000)) / 1000,
    calcium = (.data$Calcium_mu * (.data$catch * 1000) / 1000),
    iron = (.data$Iron_mu * (.data$catch * 1000)) / 1000,
    vitaminA = (.data$Vitamin_A_mu * (.data$catch * 1000)) / 1000
  )

trips <- get_merged_trips(pars)
kobo_trips <-
  trips %>%
  dplyr::mutate(
    landing_period = lubridate::floor_date(landing_date, unit = "month"),
    landing_id = as.character(landing_id),
    n_fishers = fisher_number_man + fisher_number_woman + fisher_number_child) %>%
  tidyr::unnest(.data$landing_catch) %>%
  tidyr::unnest(.data$length_frequency) %>%
  dplyr::filter(!is.na(.data$weight)) %>%
  dplyr::group_by(.data$landing_id, .data$landing_period) %>%
  dplyr::summarise(
    landing_id = dplyr::first(landing_id),
    landing_date = dplyr::first(landing_date),
    trip_duration = dplyr::first(trip_duration),
    n_fishers = dplyr::first(n_fishers),
    habitat = dplyr::first(habitat),
    gear_type = dplyr::first(gear_type),
    vessel_type = dplyr::first(vessel_type),
    dplyr::across(
      c(.data$weight:.data$Vitamin_A_mu),
      ~ sum(.x) / 1000
    )
  ) %>%
  dplyr::ungroup()

##
usethis::use_data(RDI_tab, overwrite = TRUE)
usethis::use_data(catch_groups, overwrite = TRUE)
usethis::use_data(timor_population, overwrite = TRUE)
usethis::use_data(nutrients_table, overwrite = TRUE)
usethis::use_data(region_stats, overwrite = TRUE)
usethis::use_data(kobo_trips, overwrite = TRUE)
