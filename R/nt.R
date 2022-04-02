#' Functions to process datasets: NT
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nt <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "NT"

  # process datasets
  switch(
    uuid,
    "e008ba6f-7b09-4af4-afc5-63ec3c3bbfbb" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements(".metric") %>%
                {.[grep("Confirmed Cases", .)]} %>%
                rvest::html_text() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
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
                rvest::html_elements(".metric") %>%
                {.[grep("Deaths", .)]} %>%
                rvest::html_text() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
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
                rvest::html_elements(".metric") %>%
                {.[grep("Resolved Cases", .)]} %>%
                rvest::html_text() %>%
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
    "9ed0f5cd-2c45-40a1-94c9-25b0c9df8f48" = {
      switch(
        val,
        "cases" = {
         switch(
           fmt,
           "prov_cum_current" = {
             ds %>%
               rvest::html_element("#confirmed") %>%
               rvest::html_text2() %>%
               readr::parse_number() %>%
               data.frame(value = .) %>%
               helper_cum_current(loc = "prov", val, prov, date_current)
           },
           "subhr_cum_current_residents_nonresidents" = {
             hr <- "Northwest Territories" # in the CCODWG dataset, this would be "NWT"
             # special format - includes values for residents and non-residents
             ds %>%
               rvest::html_table(header = FALSE) %>%
               `[[`(1) %>%
               dplyr::slice(-1, -2) %>%
               dplyr::rename(
                 sub_region_2 = .data$X1,
                 active = .data$X2,
                 cases_cumulative_residents = .data$X3,
                 cases_cumulative_nonresidents = .data$X4
               ) %>%
               dplyr::select(
                 .data$sub_region_2,
                 .data$cases_cumulative_residents,
                 .data$cases_cumulative_nonresidents
               ) %>%
               tidyr::pivot_longer(
                 cols = !.data$sub_region_2,
                 names_to = "name"
               ) %>%
               helper_cum_current(loc = "subhr", val = NULL, prov, date_current, hr)
           },
           e_fmt()
         )
        },
        "active" = {
          switch(
            fmt,
            "subhr_current" = {
              hr <- "Northwest Territories" # in the CCODWG dataset, this would be "NWT"
              # special format - includes values for residents and non-residents
              ds %>%
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::slice(-1, -2) %>%
                dplyr::rename(
                  sub_region_2 = .data$X1,
                  active = .data$X2,
                  cases_cumulative_residents = .data$X3,
                  cases_cumulative_nonresidents = .data$X4
                ) %>%
                dplyr::select(
                  .data$sub_region_2,
                  .data$active
                ) %>%
                tidyr::pivot_longer(
                  cols = !.data$sub_region_2,
                  names_to = "name"
                ) %>%
                helper_cum_current(loc = "subhr", val = NULL, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#cases_deaths") %>%
                rvest::html_text2() %>%
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
                rvest::html_element("#recovered") %>%
                rvest::html_text2() %>%
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
    "66fbe91e-34c0-4f7f-aa94-cf6c14db0158" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#completed_tests") %>%
                rvest::html_text2() %>%
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
    "454de458-f7b4-4814-96a6-5a426f8c8c60" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#totaldoses") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
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
                rvest::html_element("#totalseconddose") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
                readr::parse_number() %>%
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
                rvest::html_element("#totalthirddose") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
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
