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
    "009dab7c-df60-4c48-8bbe-c666dbd0ff74" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$tot_tests) %>%
                dplyr::rename(value = .data$tot_tests) %>%
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
    "369d8d34-c8f7-4594-8e5c-7c4ccc00a7a4" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  region = "NS",
                  date = lubridate::date(as.POSIXct(.data$XCOLLDT / 1000, origin = "1970-01-01",
                                                    tz = "America/Halifax")),
                  value = .data$PCR
                ) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
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
