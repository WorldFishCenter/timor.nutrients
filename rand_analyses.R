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

