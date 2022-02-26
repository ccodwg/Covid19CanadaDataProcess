#' Functions to process datasets: CAN
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_can <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {
  # function: province abbreviation to province name (PHAC) or vice versa
  phac_prov <- function(p, mode = c("to_phac", "from_phac")) {
    match.arg(mode, choices = c("to_phac", "from_phac"), several.ok = FALSE)
    if (mode == "to_phac") {
      dplyr::case_when(
        # p == "CAN" ~ "Canada", # to use, must strip beginning of phrase
        p == "NL" ~ "Newfoundland and Labrador",
        p == "PE" ~ "Prince Edward Island",
        p == "NS" ~ "Nova Scotia",
        p == "NB" ~ "New Brunswick",
        p == "QC" ~ "Quebec",
        p == "ON" ~ "Ontario",
        p == "MB" ~ "Manitoba",
        p == "SK" ~ "Saskatchewan",
        p == "AB" ~ "Alberta",
        p == "BC" ~ "British Columbia",
        p == "YT" ~ "Yukon",
        p == "NT" ~ "Northwest Territories",
        p == "NU" ~ "Nunavut"#,
        # p == "" ~ "Federal allocation", # to use, must strip anything after (e.g., footnote)
      )
    } else {
      dplyr::case_when(
        p == "Canada" ~ "CAN",
        p == "Newfoundland and Labrador" ~ "NL",
        p == "Prince Edward Island" ~ "PE",
        p == "Nova Scotia" ~ "NS",
        p == "New Brunswick" ~ "NB",
        p == "Quebec" ~ "QC",
        p == "Ontario" ~ "ON",
        p == "Manitoba" ~ "MB",
        p == "Saskatchewan" ~ "SK",
        p == "Alberta" ~ "AB",
        p == "British Columbia" ~ "BC",
        p == "Yukon" ~ "YT",
        p == "Northwest Territories" ~ "NT",
        p == "Nunavut" ~ "NU",
        p == "Repatriated travellers" ~ "RT"
      )
    }
  }
  # process datasets
  switch(
    uuid,
    "f7db31d0-6504-4a55-86f7-608664517bdb" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$prname, .data$date, .data$numtotal) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  prname = phac_prov(.data$prname, "from_phac")) %>%
                dplyr::rename(
                  province = .data$prname,
                  value = .data$numtotal) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$prname, .data$date, .data$numdeaths) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  prname = phac_prov(.data$prname, "from_phac")) %>%
                dplyr::rename(
                  province = .data$prname,
                  value = .data$numdeaths) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$prname, .data$date, .data$numrecover) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  prname = phac_prov(.data$prname, "from_phac")) %>%
                dplyr::rename(
                  province = .data$prname,
                  value = .data$numrecover) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "testing" = {
          match.arg(testing_type, c("n_people_tested", "n_tests_completed"))
          switch(
            fmt,
            "prov_ts" = {
              if (testing_type == "n_people_tested") {
                ds %>%
                  dplyr::select(.data$prname, .data$date, .data$numtested) %>%
                  dplyr::mutate(
                    date = as.Date(.data$date),
                    prname = phac_prov(.data$prname, "from_phac")) %>%
                  dplyr::rename(
                    province = .data$prname,
                    value = .data$numtested) %>%
                  helper_ts_can(val, convert_to_cum = FALSE)
              } else if (testing_type == "n_tests_completed") {
                ds %>%
                  dplyr::select(.data$prname, .data$date, .data$numtests) %>%
                  dplyr::mutate(
                    date = as.Date(.data$date),
                    prname = phac_prov(.data$prname, "from_phac")) %>%
                  dplyr::rename(
                    province = .data$prname,
                    value = .data$numtests) %>%
                  helper_ts_can(val, convert_to_cum = FALSE)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "fa3f2917-6553-438c-9a6f-2af8d077f47f" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Vaccine distribution", .)]]} %>%
                dplyr::filter(.data$`Vaccine distribution` == phac_prov(prov, "to_phac")) %>%
                dplyr::pull(.data$Total) %>%
                gsub(" ", "", .) %>% # strip out extra spaces after commas
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    e_uuid()
  )
}
