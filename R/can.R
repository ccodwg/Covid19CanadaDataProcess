#' Functions to process datasets: CAN
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_can <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # process datasets
  switch(
    uuid,
    "d0bfcd85-9552-47a5-a699-aa6fe4815e00" = {
      switch(
        val,
        "vaccine_coverage_dose_1" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$proptotal_atleast1dose) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = TRUE)
            },
            e_fmt()
          )
        },
        "vaccine_coverage_dose_2" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$proptotal_fully) %>%
                # fix values
                dplyr::mutate(
                  value = dplyr::case_when(
                    .data$value == "" ~ "0",
                    .data$value == "<0.01" ~ "0",
                    TRUE ~ .data$value)) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = TRUE)
            },
            e_fmt()
          )
        },
        "vaccine_coverage_dose_3" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$proptotal_1additional) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = TRUE)
            },
            e_fmt()
          )
        },
        "vaccine_coverage_dose_4" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$proptotal_2nd_additional) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = TRUE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_total_doses" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::rowwise() %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = sum(
                    .data$numtotal_atleast1dose,
                    .data$numtotal_fully,
                    .data$numtotal_1additional,
                    .data$num18plus_2nd_additional,
                    na.rm = TRUE)) %>%
                dplyr::ungroup() %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_1" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$numtotal_atleast1dose) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_2" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$numtotal_fully) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_3" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$numtotal_1additional) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_4" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$week_end),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = .data$numtotal_2nd_additional) %>%
                helper_ts_can(val, convert_to_cum = FALSE, val_numeric = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f7db31d0-6504-4a55-86f7-608664517bdb" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$prname, .data$date, .data$numtotal, .data$update) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  prname = phac_prov(.data$prname, "from_phac")) %>%
                dplyr::rename(
                  region = .data$prname,
                  value = .data$numtotal) %>%
                helper_ts_can(val, convert_to_cum = FALSE, keep_update = TRUE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$prname, .data$date, .data$numdeaths, .data$update) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  prname = phac_prov(.data$prname, "from_phac")) %>%
                dplyr::rename(
                  region = .data$prname,
                  value = .data$numdeaths) %>%
                helper_ts_can(val, convert_to_cum = FALSE, keep_update = TRUE)
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
                  region = .data$prname,
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
                    region = .data$prname,
                    value = .data$numtested) %>%
                  helper_ts_can(val, convert_to_cum = FALSE)
              } else if (testing_type == "n_tests_completed") {
                ds %>%
                  dplyr::select(.data$prname, .data$date, .data$numtests) %>%
                  dplyr::mutate(
                    date = as.Date(.data$date),
                    prname = phac_prov(.data$prname, "from_phac")) %>%
                  dplyr::rename(
                    region = .data$prname,
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
    "c4631a95-cb44-4d21-ae39-f1ad54daf814" = {
      switch(
        val,
        "vaccine_administration_total_doses" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$as_of_date),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = as.integer(ifelse(.data$numtotal_all_administered == "", 0, .data$numtotal_all_administered))) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_1" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$as_of_date),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = as.integer(ifelse(.data$numtotal_dose1_administered == "", 0, .data$numtotal_dose1_administered))) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_2" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$as_of_date),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = as.integer(ifelse(.data$numtotal_dose2_administered == "", 0, .data$numtotal_dose2_administered))) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_3" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$as_of_date),
                  region = phac_prov(.data$prename, "from_phac"),
                  value = as.integer(ifelse(.data$numtotal_dose3_administered == "", 0, .data$numtotal_dose3_administered))) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
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
    "558ccc64-799f-4329-95a4-f944dac21eb1" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "can_ts" = {
              ds %>%
                dplyr::select(.data$new, .data$date) %>%
                dplyr::mutate(
                  date = as.Date(.data$date),
                  region = "CAN") %>%
                dplyr::rename(
                  value = .data$new) %>%
                helper_ts_can(val, convert_to_cum = TRUE)
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
