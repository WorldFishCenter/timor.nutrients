get_nutrients_table_legacy <- function(pars, summarise = TRUE, convert = TRUE) {
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

  nutrients_tab %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ID = digest::digest(
      paste(interagency_code,
        Selenium_mu,
        sep = "_"
      ),
      algo = "md5"
    )) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::first(.x))) %>%
    dplyr::select(-ID) %>%
    dplyr::ungroup()
}

get_nutrients_table <- function(pars, summarise = TRUE, convert = TRUE) {
  rfish_tab <- get_rfish_table(pars)
  # get invertebrates nutrients
  fao_groups <- get_fao_composition()

  nutrients_tab <-
    rfishbase::estimate(rfish_tab$Species) %>% # get updated nutrients values
    dplyr::select(!dplyr::contains("_")) %>%
    dplyr::select(.data$SpecCode, .data$Calcium:.data$Zinc) %>%
    dplyr::right_join(rfish_tab) %>%
    dplyr::select(.data$interagency_code, .data$SpecCode, .data$Calcium:.data$Zinc) %>%
    # na.omit() %>%
    dplyr::group_by(.data$interagency_code, .data$SpecCode) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::first(.x))) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      .data$interagency_code,
      Selenium_mu = .data$Selenium,
      Zinc_mu = .data$Zinc,
      Protein_mu = .data$Protein,
      Omega_3_mu = .data$Omega3,
      Calcium_mu = .data$Calcium,
      Iron_mu = .data$Iron,
      Vitamin_A_mu = .data$VitaminA
    ) %>%
    dplyr::filter(!.data$interagency_code %in% unique(fao_groups$interagency_code)) %>%
    dplyr::bind_rows(fao_groups) %>%
    dplyr::filter(!.data$interagency_code == "FLY")

  # use predicted FLY nutrient values from
  fly_group <-
    dplyr::tibble(
      Selenium_mu = 40.6528,
      Zinc_mu = 1.44887,
      Protein_mu = 17.09566,
      Omega_3_mu = 0.2460711,
      Calcium_mu = 423.1434,
      Iron_mu = 2.109762,
      Vitamin_A_mu = 25.77763,
      interagency_code = "FLY"
    )

  nutrients_tab <-
    nutrients_tab %>%
    dplyr::bind_rows(fly_group)

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

  nutrients_tab %>%
    dplyr::mutate_at(
      dplyr::vars(.data$Selenium_mu:.data$Vitamin_A_mu),
      ~ tidyr::replace_na(., stats::median(., na.rm = TRUE))
    )
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

get_fao_composition <- function() {
  fao_comp <- readr::read_csv("https://github.com/WorldFishCenter/timor.nutrients/raw/main/inst/fao_food_composition.csv")

  octopus <- c("OCT", "OCT")
  squids <- c("SQZ", "SQR", "OMZ", "CTL", "CTC")
  cockles <- c("CLV", "SVE")
  shrimps <- c("CSH", "PAL", "PAN", "PRA", "PEZ", "ENS", "MPM", "MPN", "PRB", "WKP", "PBA", "GIT", "TIP", "PNV", "SHS")
  crabs <- c("CAD", "DUN", "CRE", "PCR", "SWM", "CRB", "SCD", "MUD")
  lobsters <- c("NEX", "LBA", "LBE", "NEP", "VLO", "LOR")

  fao_comp %>%
    dplyr::rename(interagency_code = .data$integragency_code) %>%
    dplyr::filter(.data$food_state == "r") %>%
    dplyr::filter(.data$interagency_code %in% c(octopus, squids, cockles, shrimps, crabs, lobsters)) %>%
    dplyr::mutate(interagency_code = dplyr::case_when(
      .data$interagency_code %in% octopus ~ "OCZ",
      .data$interagency_code %in% squids ~ "IAX",
      .data$interagency_code %in% cockles ~ "COZ",
      .data$interagency_code %in% shrimps ~ "PEZ",
      .data$interagency_code %in% crabs ~ "CRA",
      .data$interagency_code %in% lobsters ~ "SLV",
      TRUE ~ .data$interagency_code
    )) %>%
    # dplyr::group_by(.data$interagency_code) %>%
    # dplyr::summarise(dplyr::across(.data$`protein(g)`:.data$`omega3(g)`, ~ median(.x, na.rm = TRUE))) %>%
    dplyr::rename(
      Protein_mu = .data$`protein(g)`,
      Calcium_mu = .data$`calcium(mg)`,
      Iron_mu = .data$`iron(mg)`,
      Zinc_mu = .data$`zinc(mg)`,
      Selenium_mu = .data$`selenium(mcg)`,
      Vitamin_A_mu = .data$`vitaminA(mcg)`,
      Omega_3_mu = .data$`omega3(g)`
    ) %>%
    dplyr::select(.data$interagency_code, .data$Protein_mu:.data$Omega_3_mu)
}


get_merged_trips <- function(pars, ...) {
  trips <-
    cloud_object_name(
      prefix = "all_trips",
      provider = pars$storage$google$key,
      options = pars$storage$google$options
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
