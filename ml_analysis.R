library(DataExplorer)

df_field <-
  readr::read_csv("ml_data.csv") %>%
  dplyr::mutate_all(as.factor)


#DataExplorer::plot_intro(df)
#DataExplorer::plot_bar(df)


# splitting and resampling
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
  recipes::step_integer(recipes::all_nominal())

# define model
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

doParallel::registerDoParallel(cores = 6)
set.seed(234)
xgb_res <- tune::tune_grid(
  xgb_wf,
  resamples = folds,
  grid = xgb_grid,
  control = tune::control_grid(save_pred = TRUE)
)


# dysplay tuning parameters
#xgb_res %>%
#  tune::collect_metrics() %>%
#  dplyr::filter(.metric == "roc_auc") %>%
#  dplyr::select(mean, mtry:sample_size) %>%
#  tidyr::pivot_longer(mtry:sample_size,
#                      names_to = "parameter",
#                      values_to = "value"
#  ) %>%
#  ggplot(aes(value, mean, color = parameter)) +
#  geom_point(show.legend = FALSE) +
#  facet_wrap(~parameter, scales = "free_x")


#tune::show_best(xgb_res, "roc_auc")

# select best tune
best_auc <- tune::select_best(xgb_res, "roc_auc")


final_xgb <- tune::finalize_workflow(xgb_wf, best_auc)

#final_xgb %>%
#  fit(data = train) %>%
#  hardhat::extract_fit_parsnip() %>%
#  vip::vip(geom = "point")

final_rs <- tune::last_fit(final_xgb, df_split,
                           metrics = yardstick::metric_set(accuracy, roc_auc, sens, spec)
)

#final_rs %>%
#  tune::collect_metrics()

cmat <-
  final_rs %>%
  tune::collect_predictions() %>%
  yardstick::conf_mat(cluster, .pred_class)

#summary(cmat)

# show roc curves
final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_curve(cluster, c(.pred_1:.pred_5), event_level = "second") %>%
  ggplot(aes(1-specificity, sensitivity, color = .level))+
  theme_minimal()+
  geom_line()+
  #geom_point()+
  scale_color_viridis_d()+
  labs(color = "cluster")

# show auc values
final_rs %>%
  tune::collect_predictions() %>%
  yardstick::roc_auc(cluster, c(.pred_1:.pred_5))


####

df_field_prep <-
  recipes::bake(
  recipes::prep(cust_rec),
  recipes::has_role("predictor"),
  new_data = df_field,
  composition = "matrix"
)

names(as.data.frame(df_field_prep))
names(df_field)


fit2 <-
  final_xgb %>%
  fit(data = train)


feat_names <- c("quarter", "habitat_gear", "habitat", "gear_type", "vessel_type")
simple_fit <-
  final_xgb %>%
  fit(data = train)

# Set up parallel backend
doParallel::registerDoParallel(cores = 6)

ks <- kernelshap::kernelshap(
  simple_fit,
  X = train,    # Assuming random row order
  bg_X = head(train, 500),  # Assuming random row order
  type = "prob",              # Predictions must be numeric
  feature_names = feat_names,
  parallel = TRUE,
  verbose = TRUE
)

we <- shapviz::shapviz(ks)
shapviz::sv_importance(we)+
  theme_minimal()


p1 <- sv_dependence(we, "habitat")

shapviz::shapviz(p1)
p1$data$habitat
p1$data$gear_type
p1$data$shap

p1$patches$plots
peppe <-
  dplyr::tibble(habitat = p1$data$habitat,
                gear = p1$data$gear_type,
                shap = p1$data$shap)

peppe %>%
ggplot(aes(habitat, shap, color = gear))+
  geom_boxplot()+
  labs(y = "SHAP value")+
  theme_minimal()
