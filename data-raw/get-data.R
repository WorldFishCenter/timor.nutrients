library(magrittr)

pars <- read_config()

palettes <- list(
  nutrients_palette = pars$nutrients$pal_nutrients,
  clusters_palette = pars$nutrients$pal_clusters
)

usethis::use_data(palettes, overwrite = TRUE)

RDI_tab <-
  tidyr::tibble(
    nutrient = c(
      "selenium",
      "zinc",
      "protein",
      "omega3",
      "calcium",
      "iron",
      "vitaminA"
    ),
    conv_factor = c(0.000026, 0.0049, 46, 2.939, 1, 0.0294, 0.0005)
  )

timor_population <-
  readr::read_csv(system.file(
    "timor_census_2022.csv",
    package = "timor.nutrients"
  )) %>%
  dplyr::filter(gender == "Female") %>%
  dplyr::group_by(region) %>%
  dplyr::filter(age > 14 & age < 50) %>%
  dplyr::summarise(population = sum(count, na.rm = T)) %>%
  dplyr::add_row(region = "All", population = sum(.$population)) %>%
  dplyr::ungroup()

catch_groups <-
  readr::read_csv(system.file(
    "catch_groups.csv",
    package = "timor.nutrients"
  )) %>%
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

region_data <- readr::read_rds(system.file(
  "estimations_kg_12_2023_v2.rds",
  package = "timor.nutrients"
))


region_stats <-
  region_data %>%
  purrr::map(
    ~ purrr::keep(
      .x,
      stringr::str_detect(
        names(.x),
        stringr::fixed("taxa")
      )
    )
  ) %>%
  purrr::list_flatten(name_spec = "{outer}") %>%
  dplyr::bind_rows(.id = "region") %>%
  dplyr::rename(date_bin_start = .data$landing_period) %>%
  dplyr::select(c(
    .data$region,
    .data$date_bin_start,
    .data$grouped_taxa,
    .data$catch
  )) %>%
  dplyr::left_join(nutrients_table, by = "grouped_taxa") %>%
  dplyr::mutate(dplyr::across(c(Selenium_mu:Vitamin_A_mu), ~ .x * catch)) %>%
  rename_nutrients_mu()

region_stats_adj <-
  region_stats |>
  dplyr::filter(date_bin_start < "2024-01-01") |>
  dplyr::left_join(edible_portions, by = "grouped_taxa") |>
  dplyr::mutate(
    dplyr::across(
      .cols = catch:vitaminA,
      .fns = ~ .x * ep_coeff
    )
  ) |>
  dplyr::select(-ep_coeff)

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

#trips <-
#  get_merged_trips(pars) %>%
#  dplyr::filter(!is.na(landing_id))

kobo_trips <-
  trips %>%
  dplyr::mutate(
    landing_period = lubridate::floor_date(landing_date, unit = "month"),
    landing_id = as.character(landing_id),
    n_fishers = fisher_number_man + fisher_number_woman + fisher_number_child
  ) %>%
  dplyr::filter(
    landing_period >= "2018-01-01" & landing_period <= "2023-12-31",
    reporting_region %in% c("Dili", "Atauro")
  ) %>%
  tidyr::unnest(.data$landing_catch) %>%
  tidyr::unnest(.data$length_frequency) %>%
  dplyr::filter(!is.na(.data$catch)) %>%
  dplyr::group_by(.data$landing_id, .data$landing_period) %>%
  dplyr::summarise(
    landing_id = dplyr::first(landing_id),
    reporting_region = dplyr::first(municipality),
    landing_date = dplyr::first(landing_date),
    trip_duration = dplyr::first(trip_length),
    n_fishers = dplyr::first(n_fishers),
    habitat = dplyr::first(habitat),
    gear_type = dplyr::first(gear),
    mesh_size = dplyr::first(mesh_size),
    vessel_type = dplyr::first(propulsion_gear),
    landing_value = dplyr::first(landing_catch_price),
    dplyr::across(
      c(.data$catch:.data$Vitamin_A_mu),
      ~ sum(.x) / 1000
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(habitat = ifelse(habitat == "Traditional FAD", "FAD", habitat))

#catch_data <-
#  trips %>%
#  dplyr::filter(landing_id %in% kobo_trips$landing_id) %>%
#  dplyr::select(landing_id, landing_value, landing_catch)

usethis::use_data(RDI_tab, overwrite = TRUE)
usethis::use_data(catch_groups, overwrite = TRUE)
usethis::use_data(timor_population, overwrite = TRUE)
usethis::use_data(nutrients_table, overwrite = TRUE)
usethis::use_data(region_stats, overwrite = TRUE)
usethis::use_data(region_stats_adj, overwrite = TRUE)
usethis::use_data(kobo_trips, overwrite = TRUE)
usethis::use_data(catch_data, overwrite = TRUE)
devtools::document()

# get adjusted map data
df <-
  timor.nutrients::region_stats_adj |>
  dplyr::group_by(region, date_bin_start) %>%
  dplyr::summarise(dplyr::across(is.numeric, ~ sum(.)), .groups = "drop") %>%
  dplyr::mutate(year = lubridate::year(date_bin_start)) %>%
  dplyr::filter(year %in% c(2020, 2021, 2022)) |>
  dplyr::group_by(year, region) |>
  dplyr::summarise(
    dplyr::across(c(catch:vitaminA), sum, na.rm = T),
    .groups = "drop"
  ) |>
  dplyr::group_by(region) |>
  dplyr::summarise(
    dplyr::across(c(catch:vitaminA), mean, na.rm = T),
    .groups = "drop"
  ) |>
  dplyr::mutate(catch = catch / 1000)

df_total <-
  df |>
  dplyr::bind_rows(
    df |>
      dplyr::summarise(
        region = "National",
        dplyr::across(where(is.numeric), sum, na.rm = T)
      )
  )

# Join the tables and calculate WRA supplied for each nutrient
wra_supplied <-
  df_total |>
  dplyr::select(-catch) |>
  tidyr::pivot_longer(
    cols = -region,
    names_to = "nutrient",
    values_to = "kg_annual"
  ) |>
  dplyr::left_join(rdi_table, by = "nutrient") |>
  dplyr::mutate(wra_supplied = kg_annual / conv_factor) |>
  dplyr::select(region, nutrient, wra_supplied) |>
  tidyr::pivot_wider(names_from = nutrient, values_from = wra_supplied) |>
  dplyr::select(-selenium) |>
  dplyr::rowwise() |>
  dplyr::mutate(total = sum(dplyr::c_across(zinc:vitaminA), na.rm = T)) |>
  dplyr::ungroup() |>
  dplyr::left_join(timor_population) |>
  dplyr::mutate(
    population = dplyr::case_when(
      region == "National" ~ sum(population, na.rm = T),
      TRUE ~ population
    )
  ) |>
  dplyr::mutate(
    perc = (total / population) * 100,
    number_of_people = population * (perc / 100)
  ) |>
  dplyr::select(region, population, total, number_of_people, perc) |>
  dplyr::arrange(desc(perc))

map_data_adj <-
  df_total |>
  dplyr::select(region, catch) |>
  dplyr::left_join(wra_supplied, by = "region") |>
  dplyr::select(
    region,
    catch,
    WRA = population,
    percentage = perc,
    number = number_of_people
  ) |>
  dplyr::select(area = region, annual_catch = catch, WRA = percentage)

usethis::use_data(map_data_adj, overwrite = TRUE)

# run analyses
data_list <- get_model_data()

#permanova
set.seed(555)
data_clusters <-
  list(
    timor_GN_perm = dplyr::slice_sample(
      data_list$data_raw$timor_GN_raw,
      prop = .75
    ),
    timor_AG_perm = dplyr::slice_sample(
      data_list$data_raw$timor_AG_raw,
      prop = .75
    )
  )
perm_results <- purrr::imap(
  data_clusters,
  ~ run_permanova_clusters(.x, permutations = 999, parallel = 13)
)
usethis::use_data(perm_results, overwrite = T)
devtools::document()

#Run XGBoost model
data_list <- get_model_data()$data_processed
model_outputs <-
  purrr::imap(
    data_list,
    ~ run_xgmodel(
      dataframe = .x$dataframe,
      step_other = .x$step_other,
      n_cores = 13
    )
  ) %>%
  setNames(paste0("model_", names(.)))

usethis::use_data(model_outputs, overwrite = TRUE)
devtools::document()


#Get shap values
shap_results <- purrr::map(
  timor.nutrients::model_outputs,
  run_kernelshap,
  parallel = TRUE,
  cores = 13
)
usethis::use_data(shap_results, overwrite = T)
devtools::document()
