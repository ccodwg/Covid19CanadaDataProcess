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
                  value = .data$proptotal_additional) %>%
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
                    .data$numtotal_additional,
                    .data$numtotal_2nd_additional,
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
                  value = .data$numtotal_additional) %>%
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
    "366ce221-c1c9-4f41-a917-8ff4648f6a40" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  region = phac_prov(.data$prname, "from_phac"),
                  date = as.Date(.data$date),
                  value = .data$numtests_total,
                  .data$update
                ) %>%
                helper_ts_can(val, convert_to_cum = FALSE, keep_update = TRUE)
              },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "314c507d-7e48-476e-937b-965499f51e8e" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds <- ds %>%
                # select variables
                dplyr::transmute(
                  region = phac_prov(.data$prname, "from_phac"),
                  date = as.Date(.data$date),
                  value = as.integer(.data$numcases_total),
                  update = .data$update
                )
              # update column is all NAs for Canada, repatriated travellers
              # some other columns may be all 0s
              # convert them to columns of all 1s
              update_table <- table(ds$region, ds$update, useNA = "always")
              all_0 <- rownames(update_table[update_table[, "1"] == 0, ])
              all_0 <- all_0[!is.na(all_0)]
              ds[ds$region %in% all_0, "update"] <- 1
              # if update column is 0 for all values but one, fill in 1s for all dates before the first
              only_1 <- rownames(update_table[update_table[, "1"] == 1, ])
              for (loc in only_1) {
                date_max <- ds[ds$region == loc & ds$update == 1, "date", drop = TRUE]
                ds[ds$region == loc, "update"] <- ifelse(ds[ds$region == loc, "date"] <= date_max, 1, 0)
              }
              ds %>%
                # filter out update == 0
                dplyr::filter(.data$update == 1) %>%
                helper_ts_can(val, convert_to_cum = FALSE, keep_update = TRUE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ds <- ds %>%
                # select variables
                dplyr::transmute(
                  region = phac_prov(.data$prname, "from_phac"),
                  date = as.Date(.data$date),
                  value = as.integer(.data$numdeaths_total),
                  update = .data$update
                )
              # update column is all NAs for Canada, repatriated travellers
              # some other columns may be all 0s
              # convert them to columns of all 1s
              update_table <- table(ds$region, ds$update, useNA = "always")
              all_0 <- rownames(update_table[update_table[, "1"] == 0, ])
              all_0 <- all_0[!is.na(all_0)]
              ds[ds$region %in% all_0, "update"] <- 1
              # if update column is 0 for all values but one, fill in 1s for all dates before the first
              only_1 <- rownames(update_table[update_table[, "1"] == 1, ])
              for (loc in only_1) {
                date_max <- ds[ds$region == loc & ds$update == 1, "date", drop = TRUE]
                ds[ds$region == loc, "update"] <- ifelse(ds[ds$region == loc, "date"] <= date_max, 1, 0)
              }
              ds %>%
                # filter out update == 0
                dplyr::filter(.data$update == 1) %>%
                helper_ts_can(val, convert_to_cum = FALSE, keep_update = TRUE)
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
    "ea3718c1-83f1-46a1-8b21-e25aebd1ebee" = {
      switch(
        val,
        "wastewater_copies_per_ml" = {
          switch(
            fmt,
            "subhr_ts" = {
              # process data
              ds <- ds %>%
                dplyr::transmute(
                  name = "wastewater_copies_per_ml",
                  date = as.Date(.data$Date),
                  sub_region_1 = .data$region,
                  region = dplyr::case_when(
                    .data$region %in% c("Edmonton") ~ "AB",
                    .data$region %in% c("Vancouver") ~ "BC",
                    .data$region %in% c("Winnipeg", "Brandon") ~ "MB",
                    .data$region %in% c("Moncton") ~ "NB",
                    .data$region %in% c("St. John's") ~ "NL",
                    .data$region %in% c("Halifax") ~ "NS",
                    .data$region %in% c("Toronto") ~ "ON",
                    .data$region %in% c("Alberton", "City of Charlottetown & Town of Stratford", "Summerside") ~ "PE",
                    .data$region %in% c("Montreal") ~ "QC",
                    .data$region %in% c("North Battleford", "Prince Albert", "Regina", "Saskatoon") ~ "SK",
                    .data$region %in% c("Haines Junction") ~ "YT"
                    ),
                  sub_region_2 = .data$Location,
                  value = .data$viral_load
                  ) %>%
                dplyr::select("name", "date", "region", "sub_region_1", "sub_region_2", "value") %>%
                dplyr::arrange(.data$name, .data$region, .data$sub_region_1, .data$sub_region_2, .data$date)
              # throw error if a new location has been added and is not classified yet
              if (sum(is.na(ds$region)) > 0) {
                stop("Unrecognized region in wastewater data. Please update the function.")
              } else {
                # return data
                ds
              }
              },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f1e1a857-fab8-4c25-a132-f474fab93622" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "can_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  region = "CAN",
                  value = .data$COVID_HOSP) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "can_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  region = "CAN",
                  value = .data$COVID_ICU) %>%
                helper_ts_can(val, convert_to_cum = FALSE)
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
