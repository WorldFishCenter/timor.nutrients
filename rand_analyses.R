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

### shap values ####

sha <- shapviz::shapviz(timor.nutrients::shap_results$model_atauro_GN)
shapviz::sv_importance(sha)

shapviz::sv_dependence(sha$.pred_1,
                       v = "mesh_size",
                       color_var = "habitat",
                       color = "#008080",
                       viridis_args = list(begin = 1, end = 0, option = "turbo"),
                       jitter_width = 0.5) &
  ggplot2::theme_minimal() &
  ggplot2::scale_x_continuous(n.breaks = 10) &
  ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey50") &
  patchwork::plot_layout(ncol = 1)&
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### new plots ####

plots <-
  list(
    timor.nutrients::model_outputs$model_atauro_AG$roc_curves + ggplot2::labs(subtitle = "Atauro - All gears"),
    timor.nutrients::model_outputs$model_atauro_GN$roc_curves + ggplot2::labs(subtitle = "Atauro - Gill net"),
    timor.nutrients::model_outputs$model_timor_AG$roc_curves + ggplot2::labs(subtitle = "Mainland - All gears"),
    timor.nutrients::model_outputs$model_timor_GN$roc_curves + ggplot2::labs(subtitle = "Mainland - Gill net")
  )
plots <- lapply(plots, function(x) {
  x +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "", y = "")
})
legend_plot <- cowplot::get_legend(plots[[1]] +
                                     ggplot2::theme(
                                       legend.position = "right",
                                       legend.key.size = ggplot2::unit(0.8, "cm"),
                                       legend.title = ggplot2::element_text(size = 12)
                                     ))
combined_plots <- cowplot::plot_grid(plotlist = plots, ncol = 2, labels = "auto")

x_label <- cowplot::draw_label("1 - Specificity", x = 0.5, y = 0.05)
y_label <- cowplot::draw_label("Sensitivity", x = 0.02, y = 0.5, angle = 90)

final_plot <-
  cowplot::plot_grid(
    combined_plots,
    legend_plot,
    ncol = 2,
    rel_widths = c(1, 0.15),
    scale = 0.9
  ) +
  x_label +
  y_label

final_plot
