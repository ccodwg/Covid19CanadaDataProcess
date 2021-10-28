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
    "66fbe91e-34c0-4f7f-aa94-cf6c14db0158" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#complete") %>%
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
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#totaldoses") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_element("#totalseconddose") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
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
