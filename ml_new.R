library(ggplot2)

df_field <-
  readr::read_rds(paste0(system.file("model-outputs", package = "timor.nutrients"), "/ml_data.rds")) %>%
  dplyr::mutate_all(as.factor)


# splitting and resampling
set.seed(123)
df_split <-
  df_field %>%
  rsample::initial_split(prop = 0.8, strata = cluster)

train <- rsample::training(df_split)
test <- rsample::testing(df_split)

# Cross validation folds from training dataset
set.seed(456)
folds <- rsample::vfold_cv(train, strata = cluster)

# pre- processing
cust_rec <-
  recipes::recipe(cluster ~ ., data = train) %>%
  #  update_role(customerID, new_role = "ID") %>%
  #  step_corr(all_numeric()) %>%
  recipes::step_corr(recipes::all_numeric(), threshold = 0.7, method = "spearman") %>%
  recipes::step_zv(recipes::all_numeric()) %>% # filter zero variance
  # recipes::step_normalize(recipes::all_numeric()) %>%
  recipes::step_other(habitat, gear_type, habitat_gear) %>%
  recipes::step_dummy(recipes::all_nominal_predictors()) %>%
  themis::step_smote(cluster)

?parsnip::boost_tree
# define model
xgb_spec <- parsnip::boost_tree(
  trees = hardhat::tune(),
  tree_depth = hardhat::tune(),
  min_n = hardhat::tune(),
  loss_reduction = hardhat::tune(), ## first three: model complexity
  sample_size = hardhat::tune(),
  mtry = hardhat::tune(), ## randomness
  learn_rate = hardhat::tune(), ## step size
  stop_iter = hardhat::tune()
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
  dials::trees(),
  dials::tree_depth(),
  dials::min_n(),
  dials::loss_reduction(),
  dials::sample_prop(),
  dials::learn_rate(),
  dials::stop_iter(),
  dials::finalize(dials::mtry(), train),
  size = 20
)

members_metrics <- yardstick::metric_set(
  yardstick::accuracy,
  yardstick::roc_auc,
  yardstick::sens,
  yardstick::spec
)

doParallel::registerDoParallel(cores = 7)
set.seed(814)
xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = tune::control_grid(save_pred = TRUE),
  metrics = members_metrics
)

# dysplay tuning parameters
 xgb_res %>%
  tune::collect_metrics() %>%
  dplyr::filter(.metric == "roc_auc") %>%
  dplyr::select(mean, trees:stop_iter) %>%
  tidyr::pivot_longer(trees:stop_iter,
                      names_to = "parameter",
                      values_to = "value"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x")


tune::show_best(xgb_res, "roc_auc")

# select best tune
best_auc <- tune::select_best(xgb_res, "roc_auc")
final_xgb <- tune::finalize_workflow(xgb_wf, best_auc)

final_xgb %>%
  fit(data = train) %>%
  hardhat::extract_fit_parsnip() %>%
  vip::vip(geom = "point")

# fit
final_rs <- tune::last_fit(final_xgb, df_split, metrics = members_metrics)

# final_rs %>%
#  tune::collect_metrics()

cmat <-
  final_rs %>%
  tune::collect_predictions() %>%
  yardstick::conf_mat(cluster, .pred_class)

summary(cmat)
# show roc curves
final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_curve(cluster, c(.pred_1:.pred_5), event_level = "second") %>%
  ggplot(aes(1 - specificity, sensitivity, color = .level)) +
  theme_minimal() +
  geom_line() +
  geom_point(size = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
  coord_cartesian(expand = FALSE) +
  scale_color_viridis_d() +
  labs(color = "cluster")

# show auc value
final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_auc(cluster, c(.pred_1:.pred_5)) %>%
  janitor::clean_names() %>%
  dplyr::mutate(estimate = round(estimate, 2)) %>%
  knitr::kable()


df_field
cust_rec %>%
  recipes::prep() %>%
  recipes::bake(new_data = NULL) %>%
  count(cluster)
