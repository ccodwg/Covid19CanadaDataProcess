#' Functions to process datasets: PE
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_pe <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "PE"

  # process datasets
  switch(
    uuid,
    "68e5cbb9-0dcc-4a4f-ade0-58a0b06b1455" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(grepl("Total cases", .data$X1)) %>%
                dplyr::select(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(grepl("deaths", .data$X1)) %>%
                dplyr::select(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(grepl("Resolved cases", .data$X1)) %>%
                dplyr::select(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(grepl("tests completed", .data$X1)) %>%
                dplyr::select(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "3ff94c42-8b12-4653-a6c9-0ddd8ff343d5" = {
      switch(
        val,
        # sheet = "Total Doses"
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        # sheet = "Partially or Fully"
        # sheet = "Partially or Fully 5-11"
        "vaccine_dose_1" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        # sheet = "Fully Immunized"
        # sheet = "Fully Immunized 5-11"
        "vaccine_dose_2" = {
            switch(
              fmt,
              "prov_cum_current" = {
                ds %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(value = .) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              },
              e_fmt()
            )
        },
        # sheet = "Third Doses"
        "vaccine_additional_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
