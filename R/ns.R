#' Functions to process datasets: NS
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_ns <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "NS"

  # definitions
  ns_zones <- c(
    "Zone 1 - Western",
    "Zone 2 - Northern",
    "Zone 3 - Eastern",
    "Zone 4 - Central"
  )

  # process datasets
  switch(
    uuid,
    "d0f05ef1-419f-4f4c-bc2d-17446c10059f" = {
      switch(
       val,
       "cases" = {
         switch(
           fmt,
           "hr_cum_current" = {
             ds$features$attributes %>%
               dplyr::select(.data$ZONE_LABEL, .data$cumu_cases) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$cumu_cases
               ) %>%
               helper_cum_current(loc = "hr", val, prov, date_current)
           },
           e_fmt()
         )
       },
       "mortality" = {
         switch(
           fmt,
           "hr_cum_current" = {
             ds$features$attributes %>%
               dplyr::select(.data$ZONE_LABEL, .data$tot_deaths) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$tot_deaths
               ) %>%
               helper_cum_current(loc = "hr", val, prov, date_current)
           },
           e_fmt()
         )
       },
       "recovered" = {
         switch(
           fmt,
           "hr_cum_current" = {
             ds$features$attributes %>%
               dplyr::select(.data$ZONE_LABEL, .data$res_cases) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$res_cases
               ) %>%
               helper_cum_current(loc = "hr", val, prov, date_current)
           },
           e_fmt()
         )
       },
       e_val()
      )
    },
    "0e7a1f46-5d31-4267-be97-831172fa7081" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$tests_cumu) %>%
                dplyr::rename(value = .data$tests_cumu) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "d025e7c2-b0bd-48b8-a30f-1ae240e78e7e" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(value = .data$dose_adm) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_dose_1" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(value = .data$dose_1) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_dose_2" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(value = .data$dose_2) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_dose_3" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(value = .data$dose_3) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "7b7be246-cd65-4f35-b354-faa705cacecc" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_text2() %>%
                stringr::str_extract("(?<!\\S)(\\d*\\.?\\d+|\\d{1,3}(,\\d{3})*(\\.\\d+)?)(?!\\S)(?=[a-zA-Z\\s]* doses of COVID-19 vaccine have been administered)") %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_dose_2" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_text2() %>%
                stringr::str_extract("(?<!\\S)(\\d*\\.?\\d+|\\d{1,3}(,\\d{3})*(\\.\\d+)?)(?!\\S)(?=[a-zA-Z\\s]* second dose)") %>%
                readr::parse_number() %>%
                {round(. / 100 * 971395)} %>% # derive doses from 2019 mid-year pop and second dose coverage estimate
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_additional_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_text2() %>%
                stringr::str_extract("(?<!\\S)(\\d*\\.?\\d+|\\d{1,3}(,\\d{3})*(\\.\\d+)?)(?!\\S)(?=[a-zA-Z\\s]* third dose)") %>%
                readr::parse_number() %>%
                {round(. / 100 * 971395)} %>% # derive doses from 2019 mid-year pop and third dose coverage estimate
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "d0f05ef1-419f-4f4c-bc2d-17446c10059f" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$zone == "NS") %>%
                # hospitalizations = hospitalizations (non-ICU) + ICU
                dplyr::mutate(
                  hos = readr::parse_number(.$hos),
                  hos_icu = readr::parse_number(.$hos_icu)) %>%
                dplyr::transmute(value = .data$hos +.data$hos_icu) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$zone == "NS") %>%
                dplyr::transmute(value = readr::parse_number(.$hos_icu)) %>%
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
