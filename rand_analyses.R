### descriptive nutrients plots ####
habitat_nutrients <-
  timor.nutrients::kobo_trips %>%
  dplyr::select(habitat, weight:Vitamin_A_mu) %>%
  rename_nutrients_mu() %>%
  tidyr::pivot_longer(-c(habitat, weight), names_to = "nutrient") %>%
  dplyr::left_join(RDI_tab) %>%
  dplyr::mutate(
    value = value / weight,
    inds_kg = (value * 1000) / conv_factor
  ) %>%
  dplyr::group_by(habitat, nutrient) %>%
  dplyr::summarise(inds_kg = median(inds_kg, na.rm = T) / 10) %>%
  dplyr::mutate(inds_kg = inds_kg * 100) %>%
  dplyr::filter(!nutrient == "selenium") %>%
  dplyr::mutate(
    nutrient = stringr::str_to_title(nutrient),
    nutrient = dplyr::case_when(
      nutrient == "Omega3" ~ "Omega-3",
      nutrient == "Vitamina" ~ "Vitamin-A",
      TRUE ~ nutrient
    )
  )

ggplot2::ggplot() +
  ggplot2::theme_minimal() +
  ggchicklet::geom_chicklet(habitat_nutrients,
                            mapping = ggplot2::aes(
                              y = inds_kg,
                              x = reorder(habitat, inds_kg),
                              fill = nutrient
                            ),
                            position = ggplot2::position_stack(reverse = FALSE),
                            alpha = 0.8,
                            width = 0.8
  ) +
  ggplot2::geom_text(habitat_nutrients,
            mapping = ggplot2::aes(
              y = inds_kg,
              x = reorder(habitat, inds_kg),
              label = round(inds_kg, 1),
              group = nutrient
            ),
            position = ggplot2::position_stack(0.5, reverse = FALSE),
            color = "white",
            size = 3
  ) +
  ggplot2::scale_fill_manual(values = timor.nutrients::palettes$nutrients_palette) +
  ggplot2::scale_color_manual(values = timor.nutrients::palettes$nutrients_palette) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  ggplot2::coord_flip(expand = FALSE) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(x = "", fill = "", y = "Matched RNI from 100g portion")



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

