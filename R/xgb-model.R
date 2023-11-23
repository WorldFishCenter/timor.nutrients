#' Run XGModel
#'
#' This function facilitates the execution of an XGBoost model using a given
#' dataset. It includes processes like data splitting, cross-validation,
#' preparation of machine learning recipe, hyperparameter tuning, and model evaluation.
#' The function returns a list containing the final model fit,
#' variable importance plot, ROC curves, and AUC value.
#'
#' @param dataframe A dataframe to be used for modeling. This should contain
#' both the features and the target variable.
#' @param step_other Parameters for `recipes::step_other` function to handle
#' factor levels with few occurrences in the dataset.
#' @param n_cores Number of cores used for parallel processing in `doParallel::registerDoParallel`.
#'
#' @return A list containing various elements including the final model fit,
#' variable importance plot, ROC curves, and AUC value.
#' @export
#'
#' @examples
#' \dontrun{
#' data(iris)
#' results <- run_xgmodel(
#'   dataframe = iris,
#'   step_other = "Other",
#'   n_cores = 6
#' )
#' print(results)
#' }
run_xgmodel <- function(dataframe = NULL,
                        step_other = NULL,
                        n_cores = NULL) {
  logger::log_info("Splitting dataframe into train and test")
  set.seed(234)
  df_split <-
    dataframe %>%
    rsample::initial_split(prop = 0.8, strata = cluster)

  train <- rsample::training(df_split)
  test <- rsample::testing(df_split)

  logger::log_info("Cross validation folds from training dataset")
  set.seed(567)
  folds <- rsample::vfold_cv(train, strata = cluster)

  logger::log_info("Prepare ML recipe")
  cust_rec <-
    recipes::recipe(cluster ~ ., data = train) %>%
    #  update_role(customerID, new_role = "ID") %>%
    #  step_corr(all_numeric()) %>%
    recipes::step_corr(recipes::all_numeric(), threshold = 0.7, method = "spearman") %>%
    recipes::step_zv(recipes::all_numeric()) %>% # filter zero variance
    # recipes::step_normalize(recipes::all_numeric()) %>%
    recipes::step_other(step_other) %>%
    recipes::step_dummy(recipes::all_nominal_predictors()) %>%
    themis::step_smote(cluster)

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

  # choose model performance metrics
  members_metrics <- yardstick::metric_set(
    yardstick::accuracy,
    yardstick::roc_auc,
    yardstick::sens,
    yardstick::spec
  )

  logger::log_info("Hyperparameters optimization")
  doParallel::registerDoParallel(cores = n_cores)
  set.seed(891)
  xgb_res <- tune::tune_grid(
    xgb_wf,
    resamples = folds,
    grid = xgb_grid,
    control = tune::control_grid(save_pred = TRUE)
  )

  best_auc <- tune::select_best(xgb_res, "roc_auc")
  final_xgb <- tune::finalize_workflow(xgb_wf, best_auc)

  VI_plot <-
    final_xgb %>%
    fit(data = train) %>%
    hardhat::extract_fit_parsnip() %>%
    vip::vip(geom = "point") +
    ggplot2::theme_minimal()

  logger::log_info("Fit model with best performance")
  final_rs <- tune::last_fit(final_xgb, df_split, metrics = members_metrics)

  final_fit <-
    final_xgb %>%
    fit(data = train)

  conf_matrix <-
    final_rs %>%
    tune::collect_predictions() %>%
    yardstick::conf_mat(cluster, .pred_class)

  # show roc curves
  feat_names <-
    final_rs %>%
    tune::collect_predictions() %>%
    dplyr::select(dplyr::starts_with(".pred_")) %>%
    dplyr::select(-".pred_class") %>%
    names()

  roc_curves <-
    final_rs %>%
    tune::collect_predictions() %>%
    yardstick::roc_curve(cluster, feat_names, event_level = "second") %>%
    ggplot2::ggplot(ggplot2::aes(1 - specificity, sensitivity, color = .level)) +
    ggplot2::theme_minimal() +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "grey", linetype = "dashed") +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(color = "cluster")

  # show auc value
  auc_value <-
    final_rs %>%
    tune::collect_predictions() %>%
    yardstick::roc_auc(cluster, feat_names) %>%
    janitor::clean_names() %>%
    dplyr::mutate(estimate = round(estimate, 2))

  list(
    raw_data = dataframe,
    train_data = train,
    importance_plot = VI_plot,
    confusion_matrix = conf_matrix,
    final_model = final_xgb,
    fit = final_fit,
    roc_curves = roc_curves,
    auc_value = auc_value
  )
}

#' Run KernelSHAP on a Model
#'
#' This function applies the KernelSHAP algorithm to a given model. It uses a subset of the training data
#' to explain the predictions made by the model.
#'
#' @param model A model object that contains at least two elements: `$train_data`, a dataframe of training data,
#' and `$fit`, the fitted model object.
#'
#' @details
#' The function first removes the last column from the training data. It then selects a random subset of 500 observations
#' from this modified training dataset to serve as the background dataset for KernelSHAP. The KernelSHAP explanation
#' is then computed on the entire (modified) training dataset with these settings.
#'
#' @return
#' Returns an object of class `kernelshap`, which contains the SHAP (SHapley Additive exPlanations) values computed
#' for each feature in the training data, explaining the model output.
#'
#' @examples
#' \dontrun{
#' # Assuming `model` is a pre-trained model with the required structure
#' shap_values <- run_kernelshap(model)
#' }
#' @export
#'
run_kernelshap <- function(model) {
  train_clean <- model$train_data[-ncol(model$train_data)]
  bg_X <- dplyr::sample_n(train_clean, 500)

  kernelshap::kernelshap(
    object = model$fit,
    X = train_clean,
    bg_X = bg_X,
    type = "prob",
    feature_names = names(train_clean),
    parallel = TRUE,
    verbose = TRUE
  )
}
