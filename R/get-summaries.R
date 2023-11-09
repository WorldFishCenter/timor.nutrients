#' Generate highlight summary table
#'
#' Generate a summary table of nutritional scenario in Timor-Est by region.
#'
#' @param use_20 Wether to use 20% or 100% of assumed daily nutrient intake
#'
#' @return A interactive table
#' @export
#'
generate_summary_table <- function(use_20 = TRUE) {
  if (isTRUE(use_20)) {
    rdi_table <- timor.nutrients::RDI_tab %>% dplyr::mutate(conv_factor = conv_factor * 0.20)
  } else {
    rdi_table <- timor.nutrients::RDI_tab
  }
  municipal_nut_month <-
    timor.nutrients::region_stats %>%
    dplyr::group_by(region, date_bin_start) %>%
    dplyr::summarise(dplyr::across(is.numeric, ~ sum(.))) %>%
    dplyr::mutate(year = lubridate::year(date_bin_start)) %>%
    dplyr::select(region, year, dplyr::everything()) %>%
    dplyr::ungroup()

  nut_region <-
    municipal_nut_month %>%
    dplyr::select(-catch, -date_bin_start) %>%
    dplyr::filter(year == 2022) %>%
    dplyr::group_by(region, year) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.))) %>%
    tidyr::pivot_longer(-c(region, year), names_to = "nutrient", values_to = "kg") %>%
    dplyr::ungroup()

  nut_all <-
    nut_region %>%
    dplyr::group_by(year, nutrient) %>%
    dplyr::summarise(kg = sum(kg))

  nut_region <-
    dplyr::bind_rows(nut_region, nut_all) %>%
    dplyr::mutate(region = ifelse(is.na(region), "All", region))

  tab <-
    nut_region %>%
    dplyr::left_join(timor.nutrients::timor_population, by = "region") %>%
    dplyr::left_join(rdi_table, by = "nutrient") %>%
    dplyr::rename(
      "annual_kg" = kg,
      "region_population" = population,
      "rni" = conv_factor
    ) %>%
    dplyr::mutate(
      rni = rni / 1000,
      people_supplied_daily = (annual_kg / rni) / 365,
      percent_population_supplied = (people_supplied_daily / (region_population)) * 100,
      annual_kg = round(annual_kg, 4),
      people_supplied_daily = round(people_supplied_daily, 3),
      percent_population_supplied = round(percent_population_supplied, 3),
      region_population = format(region_population, big.mark = ","),
      region = paste0(region, " (", region_population, ")")
    ) %>%
    dplyr::select(region, nutrient, annual_kg, people_supplied_daily, percent_population_supplied)

  color_pal <- c("#f5fcdf", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")

  make_color_pal <- function(colors, bias = 1) {
    get_color <- grDevices::colorRamp(colors, bias = bias)
    function(x) grDevices::rgb(get_color(x), maxColorValue = 255)
  }

  good_color <- make_color_pal(color_pal, bias = 2)

  tab$region <- factor(tab$region, levels = unique(tab$region))

  reactable::reactable(
    tab,
    theme = reactablefmtr::fivethirtyeight(centered = TRUE),
    pagination = FALSE,
    compact = FALSE,
    borderless = FALSE,
    striped = FALSE,
    fullWidth = TRUE,
    sortable = TRUE,
    filterable = TRUE,
    groupBy = "region",
    defaultExpanded = TRUE,
    rowStyle = htmlwidgets::JS("
    function(rowInfo, state) {
      // Ignore padding rows
      if (!rowInfo) return

      // Add horizontal separators between groups when sorting by school
      const firstSorted = state.sorted[0]
      if (firstSorted && firstSorted.id === 'region') {
        const nextRow = state.pageRows[rowInfo.viewIndex + 1]
        if (nextRow && rowInfo.values['region'] !== nextRow['region']) {
          // Use box-shadow to add a 2px border without taking extra space
          return { boxShadow: 'inset 0 -2px 0 rgba(0, 0, 0, 0.1)' }
        }
      }
    }
  "),
    defaultColDef = reactable::colDef(
      align = "center",
      minWidth = 100
    ),
    columns = list(
      region = reactable::colDef(
        name = "Municipality (population)",
        sortable = FALSE,
        minWidth = 140,
        align = "center",
        style = htmlwidgets::JS("function(rowInfo, column, state) {
        const firstSorted = state.sorted[0]
        if (!firstSorted || firstSorted.id === 'region') {
          const prevRow = state.pageRows[rowInfo.viewIndex - 1]
          if (prevRow && rowInfo.values['region'] === prevRow['region']) {
            return { visibility: 'hidden' }
          }
        }
      }"),
      ),
      nutrient = reactable::colDef(
        name = "Nutrient"
      ),
      annual_kg = reactable::colDef(
        name = "Annual supply (Kg)",
        format = reactable::colFormat(separators = TRUE),
        style = function(value) {
          normalized <- (log(value + 1) - min(log(tab$annual_kg + 1))) / (max(log(tab$annual_kg + 1)) - min(log(tab$annual_kg + 1)))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      people_supplied_daily = reactable::colDef(
        name = "N. people supplied daily",
        format = reactable::colFormat(separators = TRUE),
        style = function(value) {
          normalized <- (log(value + 1) - min(log(tab$people_supplied_daily + 1))) / (max(log(tab$people_supplied_daily + 1)) - min(log(tab$people_supplied_daily + 1)))
          color <- good_color(normalized)
          list(background = color)
        },
      ),
      percent_population_supplied = reactable::colDef(
        name = "Population meeting RNI requirements",
        format = reactable::colFormat(suffix = "%", separators = TRUE),
        html = TRUE,
        style = function(value) {
          normalized <- (log(value + 1) - min(log(tab$percent_population_supplied + 1))) / (max(log(tab$percent_population_supplied + 1)) - min(log(tab$percent_population_supplied + 1)))
          color <- good_color(normalized)
          list(background = color)
        },
        # header = htmlwidgets::JS('function(column) {return `<div style="color: #6565bf">${column.name}</div>`}')
      )
    ),
    outlined = TRUE
  )
}
