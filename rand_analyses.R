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


##### ml #######
# these are the cluster data (from section nutrients distrivution)
library(DataExplorer)
df <- readr::read_csv("ml_data.csv")

DataExplorer::plot_intro(df)
DataExplorer::plot_bar(df[, -1])

# wrangling
df_field <- df %>%
  dplyr::mutate_all(as.factor)

# splitting and resampling
library(tidymodels)

set.seed(123)
df_split <-
  df_field %>%
  rsample::initial_split(prop = 0.8, strata = cluster)

train <- rsample::training(df_split)
test <- rsample::testing(df_split)

# Cross validation folds from training dataset
set.seed(234)
folds <- rsample::vfold_cv(train, strata = cluster)

# pre- processing

cust_rec <-
  recipes::recipe(cluster ~ ., data = train) %>%
  #  update_role(customerID, new_role = "ID") %>%
  #  step_corr(all_numeric()) %>%
  recipes::step_corr(recipes::all_numeric(), threshold = 0.7, method = "spearman") %>%
  recipes::step_zv(recipes::all_numeric()) %>% # filter zero variance
  # recipes::step_normalize(recipes::all_numeric()) %>%
  recipes::step_dummy(recipes::all_nominal_predictors())


# Model

xgb_spec0 <-
  parsnip::boost_tree() %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode("classification")

xgb_wf0 <-
  workflows::workflow() %>%
  workflows::add_recipe(cust_rec) %>%
  workflows::add_model(xgb_spec0)

# fit model
xgb_fit0 <- tune::fit_resamples(xgb_wf0,
  resamples = folds,
  metrics = yardstick::metric_set(accuracy, roc_auc, sens, spec),
  control = tune::control_resamples(save_pred = TRUE)
)

xgb_fit0 %>%
  tune::collect_metrics()

# final fit

xgb_final <- tune::last_fit(
  xgb_wf0,
  split = df_split,
  metrics = yardstick::metric_set(accuracy, roc_auc, sens, spec)
)

xgb_final %>%
  tune::collect_metrics()

xgb_final %>%
  tune::collect_predictions() %>%
  yardstick::conf_mat(cluster, .pred_class)



#### using parameters
# Setup a model specification

xgb_spec <- parsnip::boost_tree(
  trees = 500,
  tree_depth = hardhat::tune(),
  min_n = hardhat::tune(),
  loss_reduction = hardhat::tune(), ## first three: model complexity
  sample_size = hardhat::tune(), mtry = hardhat::tune(), ## randomness
  learn_rate = hardhat::tune() ## step size
) %>%
  parsnip::set_engine("xgboost") %>%
  parsnip::set_mode("classification")

# Passing to workflow formula and Model specification
xgb_wf <-
  workflows::workflow() %>%
  workflows::add_formula(cluster ~ .) %>%
  workflows::add_model(xgb_spec)

# tuning

xgb_grid <- dials::grid_latin_hypercube(
  dials::tree_depth(),
  dials::min_n(),
  dials::loss_reduction(),
  sample_size = sample_prop(),
  dials::finalize(dials::mtry(), train),
  dials::learn_rate(),
  size = 20
)
install.packages("doParallel")
library(finetune)

doParallel::registerDoParallel(cores = 4)

set.seed(234)
xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = tune::control_grid(save_pred = TRUE)
)
xgb_res

# dysplay tuning parameters

xgb_res %>%
  tune::collect_metrics() %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry:sample_size) %>%
  tidyr::pivot_longer(mtry:sample_size,
    names_to = "parameter",
    values_to = "value"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x")


tune::show_best(xgb_res, "roc_auc")

best_auc <- tune::select_best(xgb_res, "roc_auc")


final_xgb <- tune::finalize_workflow(xgb_wf, best_auc)
final_xgb

final_xgb %>%
  fit(data = train) %>%
  hardhat::extract_fit_parsnip() %>%
  vip::vip(geom = "point")

final_rs <- tune::last_fit(final_xgb, df_split,
  metrics = yardstick::metric_set(accuracy, roc_auc, sens, spec)
)
final_rs %>%
  tune::collect_metrics()

cmat <-
final_rs %>%
  tune::collect_predictions() %>%
  yardstick::conf_mat(cluster, .pred_class)

cmat
summary(cmat)


final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_curve(cluster, c(.pred_1:.pred_5), event_level = "second") %>%
  ggplot(aes(1-specificity, sensitivity, color = .level))+
  theme_minimal()+
  geom_line()+
  geom_point()+
  scale_color_viridis_d()+
  labs(color = "cluster")


final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_auc(cluster, c(.pred_1:.pred_5))


install.packages("")
