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

  best_auc <- tune::select_best(xgb_res, metric = "roc_auc")
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

  # show auc value
  auc_value <-
    final_rs %>%
    tune::collect_predictions() %>%
    yardstick::roc_auc(cluster, feat_names) %>%
    janitor::clean_names() %>%
    dplyr::mutate(estimate = round(estimate, 2))

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
    ggplot2::scale_color_manual(values = timor.nutrients::palettes$clusters_palette) +
    ggplot2::labs(color = "cluster")


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
#' @param parallel A logical value indicating whether to use parallel processing. Defaults to `NULL`, which will not
#' use parallel processing. Set to `TRUE` to enable parallel processing.
#' @param cores An integer specifying the number of cores to use for parallel processing. Only relevant if `parallel` is `TRUE`.
#' Defaults to `NULL`, which will use the default number of cores determined by the parallel backend.#'
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
run_kernelshap <- function(model, parallel = NULL, cores = NULL) {
  train_clean <- model$train_data[-ncol(model$train_data)]
  bg_X <- dplyr::sample_n(train_clean, 500)

  doParallel::registerDoParallel(cores = cores)

  kernelshap::kernelshap(
    object = model$fit,
    X = train_clean,
    bg_X = bg_X,
    type = "prob",
    feature_names = names(train_clean),
    parallel = parallel,
    verbose = TRUE
  )
}
#' Plot SHAP Values for Different Model Types
#'
#' This function generates a scatter plot of SHAP (SHapley Additive exPlanations) values for different model types.
#' It is specifically designed for either "gn" or other model types, plotting different aspects of the data.
#'
#' @param shap_object A list containing the SHAP values and corresponding feature values. Must have elements `X` and `S`.
#' @param model_type A character string specifying the model type. For this function, it can be "gn" or other types.
#' @param alpha The alpha value for the geom_jitter layer in ggplot2, controlling the transparency of points.
#'
#' @details
#' If `model_type` is "gn", the function plots `mesh_size`, `habitat`, and their corresponding SHAP values.
#' Otherwise, it plots `habitat_gear`, `vessel_type`, and their corresponding SHAP values.
#'
#' @return A ggplot object representing the SHAP value scatter plot.
#'
#' @examples
#' \dontrun{
#' plot_shap(shap_object = my_shap_data, model_type = "gn", alpha = 0.5)
#' }
plot_shap <- function(shap_object = NULL, model_type = NULL, alpha = NULL) {
  if (model_type == "gn") {
    process_shap <-
      dplyr::tibble(
        mesh_fact = shap_object$X$mesh_size,
        habitat_fact = shap_object$X$habitat,
        mesh_shap = shap_object$S[, 3]
      )
    process_shap %>%
      ggplot2::ggplot(ggplot2::aes(mesh_fact, mesh_shap, color = habitat_fact)) +
      ggplot2::geom_jitter(width = 2, alpha = alpha, size = 1.5, show.legend = FALSE) +
      ggplot2::geom_point(size = 0.1) +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(n.breaks = 10) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
      ggplot2::scale_color_manual(values = c("#f28f3b", "#c27ba0", "#ffd5c2", "#588b8b", "#c8553d", "#2d3047", "#007ea7")) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(color = "Habitat") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.75)))
  } else {
    process_shap <-
      dplyr::tibble(
        habitat_gear_fact = as.character(shap_object$X$habitat_gear),
        vessel_fact = shap_object$X$vessel_type,
        habitat_gear_shap = shap_object$S[, 2]
      )

    to_group <-
      process_shap %>%
      dplyr::mutate(
        zero_dist = 0 - abs(habitat_gear_shap)
      ) %>%
      dplyr::group_by(habitat_gear_fact) %>%
      dplyr::summarise(zero_dist = mean(zero_dist)) %>%
      dplyr::slice_max(order_by = zero_dist, n = 10) %>%
      magrittr::extract2("habitat_gear_fact")

    process_shap <-
      process_shap %>%
      dplyr::mutate(habitat_gear_fact = ifelse(habitat_gear_fact %in% to_group, "Others", habitat_gear_fact))

    process_shap %>%
      ggplot2::ggplot(aes(reorder(habitat_gear_fact, habitat_gear_shap), habitat_gear_shap, color = vessel_fact)) +
      ggplot2::geom_jitter(width = 0.2, alpha = alpha, size = 1.5, show.legend = FALSE) +
      ggplot2::geom_point(size = 0.1) +
      ggplot2::theme_minimal() +
      ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
      ggplot2::scale_color_manual(values = c("grey50", "#bc4749")) +
      ggplot2::coord_cartesian(expand = FALSE) +
      ggplot2::labs(color = "Transport") +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.75))) +
      ggplot2::coord_flip()
  }
}

#' Plot SHAP Values for Multiple Models
#'
#' This function visualizes SHAP values across multiple models or model types, integrating the `plot_shap` function.
#'
#' @param data_shaps A data frame or list of data frames containing SHAP values and corresponding features.
#' @param model_type A character string specifying the model type, passed to the `plot_shap` function.
#' @param alpha The alpha value for geom_jitter in ggplot2, controlling point transparency.
#' @param cols The number of columns in the plot layout.
#' @param drop_legend Wether to return legend. Default is TRUE.
#'
#' @details
#' The function uses the `shapviz` package for initial processing and then applies `plot_shap` to each model.
#' It arranges the resulting plots in a grid format, optionally including a legend and common axes labels.
#'
#' @return A ggplot object representing the combined grid of SHAP value plots for different models.
#'
#' @examples
#' \dontrun{
#' plot_model_shaps(data_shaps = my_model_shaps, model_type = "gn", alpha = 0.2, cols = 2)
#' }
plot_model_shaps <- function(data_shaps = NULL, model_type = NULL, alpha = 0.2, cols = 1, drop_legend = FALSE) {
  sha <- shapviz::shapviz(data_shaps)
  shapviz_object <- purrr::map(sha, plot_shap, model_type = model_type, alpha = alpha)

  plots <- lapply(shapviz_object, function(x) {
    x +
      ggplot2::theme(
        legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        panel.grid = ggplot2::element_blank()
      ) +
      ggplot2::labs(x = "", y = "")
  })

  legend_plot <- cowplot::get_legend(plots[[1]] +
    ggplot2::theme(
      legend.position = "right",
      legend.key.size = ggplot2::unit(0.8, "cm"),
      legend.title = ggplot2::element_text(size = 12)
    ))

  combined_plots <- cowplot::plot_grid(
    plotlist = plots,
    ncol = cols,
    label_fontface = "bold",
    label_size = 9,
    hjust = 0,
    vjust = -0.5,
    align = "hv",
    labels = c("NP1", "NP2", "NP3")
  )

  if (model_type == "gn") {
    x_label <- cowplot::draw_label("Mesh size (mm)", x = 0.5, y = 0.05)
    y_label <- cowplot::draw_label("SHAP value", x = 0.015, y = 0.5, angle = 90)
  } else {
    x_label <- cowplot::draw_label("Habitat x Gear type ", x = 0.015, y = 0.5, angle = 90)
    y_label <- cowplot::draw_label("SHAP value", x = 0.5, y = 0.05)
  }


  if (drop_legend == TRUE) {
    final_plot <-
      cowplot::plot_grid(combined_plots,
        scale = 0.9,
        greedy = TRUE
      ) +
      x_label +
      y_label
  } else {
    final_plot <-
      cowplot::plot_grid(
        combined_plots,
        legend_plot,
        ncol = 2,
        rel_widths = c(1, 0.22),
        scale = 0.9,
        greedy = TRUE
      ) +
      x_label +
      y_label
  }

  final_plot
}

#' Extract SHAP Values for Specific Features
#'
#' This function extracts specific features and their corresponding SHAP values from a SHAP object.
#' It is designed to work with SHAP objects that contain features such as mesh size and habitat.
#'
#' @param shap_object A list containing the SHAP values and corresponding feature values.
#'                    Must have elements `X` and `S`, where `X` contains feature values and
#'                    `S` contains corresponding SHAP values.
#'
#' @details
#' The function specifically extracts 'mesh size' and 'habitat' features from the `X` component
#' and their corresponding SHAP values from the `S` component of the `shap_object`.
#' It is used as a utility function within other functions handling SHAP values.
#'
#' @return A `tibble` containing columns for mesh size (`mesh_fact`), habitat (`habitat_fact`),
#'         and their corresponding SHAP values (`mesh_shap`).
#'
#' @examples
#' \dontrun{
#' shap_data <- list(
#'   X = data.frame(mesh_size = 1:10, habitat = 11:20),
#'   S = matrix(runif(30), ncol = 3)
#' )
#' extracted_shaps <- get_shaps(shap_data)
#' }
get_shaps <- function(shap_object = NULL, model_type = NULL) {
  if (model_type == "gn") {
  dplyr::tibble(
    mesh_fact = shap_object$X$mesh_size,
    habitat_fact = shap_object$X$habitat,
    mesh_shap = shap_object$S[, 3]
  )
  } else {
    dplyr::tibble(
      habitat_gear_fact = as.character(shap_object$X$habitat_gear),
      vessel_fact = shap_object$X$vessel_type,
      habitat_gear_shap = shap_object$S[, 2]
    )
  }
}
