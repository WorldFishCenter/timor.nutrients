### descriptive nutrients plots ####
treemap_habitat_df <-
  timor.nutrients::kobo_trips %>%
  dplyr::filter(weight > 0) %>%
  dplyr::select(habitat, weight, Selenium_mu:Vitamin_A_mu) %>%
  na.omit() %>%
  rename_nutrients_mu() %>%
  tidyr::pivot_longer(-c(habitat, weight),
    names_to = "nutrient", values_to = "concentration_g"
  ) %>%
  dplyr::mutate(concentration_g_stand = concentration_g / weight) %>%
  dplyr::select(-weight) %>%
  dplyr::select(-concentration_g) %>%
  dplyr::group_by(habitat, nutrient) %>%
  dplyr::summarise(concentration_g_stand = median(concentration_g_stand)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(timor.nutrients::RDI_tab, by = "nutrient") %>%
  dplyr::mutate(
    concentration_g_stand = concentration_g_stand * 1000,
    nutrient = stringr::str_to_title(nutrient),
    nutrient = dplyr::case_when(
      nutrient %in% c("Selenium", "Vitamina") ~ paste(nutrient, "(μg)"),
      nutrient %in% c("Calcium", "Iron", "Zinc") ~ paste(nutrient, "(mg)"),
      TRUE ~ paste(nutrient, "(g)")
    ),
    nutrient = ifelse(nutrient == "Vitamina (μg)", "Vitamin-A (μg)", nutrient),
    nutrient = ifelse(nutrient == "Omega3 (g)", "Omega-3 (g)", nutrient)
  ) %>%
  dplyr::mutate(concentration_g_stand = (concentration_g_stand / conv_factor)) %>%
  dplyr::filter(!nutrient == "Selenium (μg)") %>%
  dplyr::mutate(nutrient = factor(nutrient, levels = c(
    "Protein (g)",
    "Zinc (mg)",
    "Calcium (mg)",
    "Omega-3 (g)",
    "Vitamin-A (μg)",
    "Iron (mg)"
  )))

treemap_habitat_df %>%
  ggplot(aes(
    area = concentration_g_stand,
    fill = nutrient,
    label = habitat,
    subgroup = nutrient
  ), alpha = 0.5) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "centre",
    grow = T,
    alpha = 0.5,
    colour = "black",
    fontface = "italic",
    min.size = 0
  ) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  scale_fill_viridis_d(direction = -1) +
  theme(legend.position = "bottom") +
  labs(fill = "")


####

treemap_gear_df <-
  timor.nutrients::kobo_trips %>%
  dplyr::filter(weight > 0) %>%
  dplyr::select(gear_type, weight, Selenium_mu:Vitamin_A_mu) %>%
  na.omit() %>%
  rename_nutrients_mu() %>%
  tidyr::pivot_longer(-c(gear_type, weight),
    names_to = "nutrient", values_to = "concentration_g"
  ) %>%
  dplyr::mutate(concentration_g_stand = concentration_g / weight) %>%
  dplyr::select(-weight) %>%
  dplyr::select(-concentration_g) %>%
  dplyr::group_by(gear_type, nutrient) %>%
  dplyr::summarise(concentration_g_stand = median(concentration_g_stand)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(timor.nutrients::RDI_tab, by = "nutrient") %>%
  dplyr::mutate(
    concentration_g_stand = concentration_g_stand * 1000,
    nutrient = stringr::str_to_title(nutrient),
    nutrient = dplyr::case_when(
      nutrient %in% c("Selenium", "Vitamina") ~ paste(nutrient, "(μg)"),
      nutrient %in% c("Calcium", "Iron", "Zinc") ~ paste(nutrient, "(mg)"),
      TRUE ~ paste(nutrient, "(g)")
    ),
    nutrient = ifelse(nutrient == "Vitamina (μg)", "Vitamin-A (μg)", nutrient),
    nutrient = ifelse(nutrient == "Omega3 (g)", "Omega-3 (g)", nutrient)
  ) %>%
  dplyr::mutate(concentration_g_stand = (concentration_g_stand / conv_factor)) %>%
  dplyr::filter(!nutrient == "Selenium (μg)")

treemap_gear_df %>%
  ggplot(aes(
    area = concentration_g_stand,
    fill = nutrient,
    label = gear_type,
    subgroup = nutrient
  ), alpha = 0.5) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(
    place = "centre",
    grow = T,
    alpha = 0.5,
    colour = "black",
    fontface = "italic",
    min.size = 0
  ) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T) +
  scale_fill_viridis_d(direction = -1) +
  theme(legend.position = "bottom") +
  labs(fill = "")



###### scores ######
pars <- read_config()
trips <- get_merged_trips(pars) %>% dplyr::filter(!is.na(landing_id))

kobo_trips <-
  trips %>%
  dplyr::mutate(
    landing_period = lubridate::floor_date(landing_date, unit = "month"),
    landing_id = as.character(landing_id),
    n_fishers = fisher_number_man + fisher_number_woman + fisher_number_child
  ) %>%
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
  dplyr::ungroup()


nutrient_score <-
  trips %>%
  dplyr::mutate(
    landing_period = lubridate::floor_date(landing_date, unit = "month"),
    landing_id = as.character(landing_id),
    n_fishers = fisher_number_man + fisher_number_woman + fisher_number_child
  ) %>%
  tidyr::unnest(.data$landing_catch) %>%
  tidyr::unnest(.data$length_frequency) %>%
  dplyr::filter(!is.na(.data$weight),
                catch_taxon %in% c("CLP", "GZP", "TUN", "SDX", "FLY")) %>% #, "CGX", "MOO", "SNA", "CJX", "BEN", "RAX", "LWX")) %>%
  dplyr::group_by(.data$landing_id, catch_taxon) %>%
  dplyr::summarise(
    landing_value = dplyr::first(landing_value),
    dplyr::across(
      c(.data$weight:.data$Vitamin_A_mu),
      ~ sum(.x) / 1000
    )
  ) %>%
  dplyr::ungroup()

we <-
nutrient_score %>%
  rename_nutrients_mu() %>%
  tidyr::pivot_longer(-c(landing_id, catch_taxon, landing_value, weight), names_to = "nutrient") %>%
  dplyr::left_join(timor.nutrients::RDI_tab) %>%
  dplyr::mutate(
    nutrients_kg_per_kg = value / weight, # standardize nutrients for 1 kg of catch
    nutrients_g_per_kg = nutrients_kg_per_kg * 1000, # convert stand nutrients in grams
    people_rni_kg = (nutrients_g_per_kg / conv_factor) * 10
  ) %>%
  dplyr::select(-c(conv_factor, nutrients_kg_per_kg, nutrients_g_per_kg)) %>%
  dplyr::filter(!nutrient == "selenium") %>%
  dplyr::group_by(landing_id, catch_taxon) %>%
  dplyr::summarise(n_score = sum(people_rni_kg),
                   landing_value = median(landing_value)) %>%
  dplyr::group_by(catch_taxon) %>%
  dplyr::mutate(n = dplyr::n()) %>%
  dplyr::group_by(catch_taxon) %>%
  dplyr::summarise(n = dplyr::first(n),
                   n_score = dplyr::first(n_score),
                   landing_value = median(landing_value, na.rm = T))

we %>%
  ggplot(aes(n_score, landing_value, alpha = log(n), color = catch_taxon)) +
  theme_minimal() +
  geom_point(size = 5)

