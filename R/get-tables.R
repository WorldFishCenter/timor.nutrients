get_nutrients_table <- function(pars, summarise = TRUE, convert = TRUE) {
  rfish_tab <- get_rfish_table(pars)

  fao_groups <- get_fao_composition()

  nutrients_tab <-
    readr::read_csv(pars$nutrients$resource,
      show_col_types = FALSE
    ) %>%
    dplyr::rename(SpecCode = .data$spec_code) %>%
    dplyr::mutate(SpecCode = as.integer(.data$SpecCode)) %>%
    dplyr::select(.data$species, .data$SpecCode, tidyselect::contains("_mu")) %>%
    dplyr::right_join(rfish_tab, by = "SpecCode") %>%
    dplyr::select(.data$interagency_code, tidyselect::contains("_mu")) %>%
    dplyr::filter(!interagency_code %in% unique(fao_groups$interagency_code)) %>%
    dplyr::bind_rows(fao_groups)

  if (isTRUE(convert)) {
    nutrients_tab <-
      nutrients_tab %>%
      dplyr::mutate(dplyr::across(
        c(.data$Zinc_mu, .data$Calcium_mu, .data$Iron_mu),
        ~ (.x / 1000) / 100
      )) %>%
      dplyr::mutate(dplyr::across(
        c(.data$Selenium_mu, .data$Vitamin_A_mu),
        ~ (.x / 1000000) / 100
      )) %>%
      dplyr::mutate(dplyr::across(
        c(.data$Omega_3_mu, .data$Protein_mu),
        ~ (.x / 1) / 100
      ))
  }
  if (isTRUE(summarise)) {
    nutrients_tab <-
      nutrients_tab %>%
      dplyr::group_by(.data$interagency_code) %>%
      dplyr::summarise_all(stats::median, na.rm = TRUE)
  }

  nutrients_tab
}

get_rfish_table <- function(pars) {
  rfish_rds <- cloud_object_name(
    prefix = "rfish-table",
    provider = pars$storage$google$key,
    extension = "rds",
    version = "latest",
    options = pars$storage$google$options,
    exact_match = TRUE
  )
  logger::log_info("Downloading {rfish_rds}...")
  download_cloud_file(
    name = rfish_rds,
    provider = pars$storage$google$key,
    options = pars$storage$google$options
  )
  readr::read_rds(file = rfish_rds)
}

get_merged_trips <- function(pars, ...) {
  trips <-
    cloud_object_name(
      prefix = "all_trips",
      provider = pars$storage$google$key,
      options = pars$storage$google$options,
      ...
    ) %>%
    download_cloud_file(
      provider = pars$storage$google$key,
      options = pars$storage$google$options
    ) %>%
    readr::read_rds()

  imei_regions <-
    trips %>%
    dplyr::filter(!is.na(.data$tracker_imei)) %>%
    dplyr::select(.data$tracker_imei, .data$reporting_region) %>%
    dplyr::group_by(.data$tracker_imei) %>%
    dplyr::count(.data$reporting_region) %>%
    dplyr::filter(!is.na(.data$reporting_region)) %>%
    dplyr::group_by(.data$tracker_imei) %>%
    dplyr::arrange(dplyr::desc(.data$n), .by_group = TRUE) %>%
    dplyr::summarise(reporting_region = dplyr::first(.data$reporting_region)) %>%
    dplyr::rename(reporting_region_fill = .data$reporting_region) %>%
    dplyr::ungroup()

  dplyr::full_join(trips, imei_regions, by = "tracker_imei") %>%
    dplyr::mutate(reporting_region = dplyr::case_when(
      is.na(.data$reporting_region) ~ .data$reporting_region_fill,
      TRUE ~ reporting_region
    )) %>%
    dplyr::select(-.data$reporting_region_fill)
}

get_fao_composition <- function() {
  fao_comp <- readr::read_csv(system.file("fao_food_composition.csv", package = "Timor.nutrients"))

  octopus <- c("OCT", "OCT")
  squids <- c("SQZ", "SQR", "OMZ", "CTL", "CTC")
  cockles <- c("CLV", "SVE")
  shrimps <- c("CSH", "PAL", "PAN", "PRA", "PEZ", "ENS", "MPM", "MPN", "PRB", "WKP", "PBA", "GIT", "TIP", "PNV", "SHS")
  crabs <- c("CAD", "DUN", "CRE", "PCR", "SWM", "CRB", "SCD", "MUD")
  lobsters <- c("NEX", "LBA", "LBE", "NEP", "VLO", "LOR")

  fao_comp %>%
    dplyr::filter(food_state == "r") %>%
    dplyr::filter(integragency_code %in% c(octopus, squids, cockles, shrimps, crabs, lobsters)) %>%
    dplyr::mutate(interagency_code = dplyr::case_when(
      integragency_code %in% octopus ~ "OCZ",
      integragency_code %in% squids ~ "IAX",
      integragency_code %in% cockles ~ "COZ",
      integragency_code %in% shrimps ~ "PEZ",
      integragency_code %in% crabs ~ "CRA",
      integragency_code %in% lobsters ~ "SLV",
      TRUE ~ integragency_code
    )) %>%
    dplyr::group_by(interagency_code) %>%
    dplyr::summarise(dplyr::across(`protein(g)`:`omega3(g)`, ~ median(.x, na.rm = TRUE))) %>%
    dplyr::rename(
      Protein_mu = `protein(g)`,
      Calcium_mu = `calcium(mg)`,
      Iron_mu = `iron(mg)`,
      Zinc_mu = `zinc(mg)`,
      Selenium_mu = `selenium(mcg)`,
      Vitamin_A_mu = `vitaminA(mcg)`,
      Omega_3_mu = `omega3(g)`
    )
}
