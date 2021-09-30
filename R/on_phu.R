#' Functions to process datasets: ON (PHU)
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_on_phu <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "ON"

  # process datasets
  switch(
    uuid,
    # Algoma
    "685df305-f6c7-4ac2-992b-ec707eb1f1cb" = {
      hr <- "Algoma"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Confirmedcases", .)[1]]]} %>%
                dplyr::select(.data$`Confirmedcases (2)`) %>%
                `[[`(1, 1) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Deceased", .)[1]]]} %>%
                dplyr::select(.data$Deceased) %>%
                `[[`(1, 1) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Resolvedcases", .)[1]]]} %>%
                dplyr::select(
                  .data$`Resolvedcases (3)`,
                  .data$Deceased) %>%
                dplyr::slice_head(n = 1) %>%
                dplyr::mutate(dplyr::across(.cols = dplyr::everything(), readr::parse_number)) %>%
                {data.frame(
                  value = .$`Resolvedcases (3)` - .$Deceased
                )} %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Northwestern
    "4c56a58b-0cb3-4d71-bafe-9fdb42e5c1d5" = {
      hr <- "Northwestern"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Totals", .)[1]]]} %>%
                dplyr::filter(.data$Region == "Totals") %>%
                dplyr::pull(.data$Total) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Deceased", .)[1]]]} %>%
                dplyr::pull(.data[["\u200BDeceased"]]) %>% # note zero-width space
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              resolved <- ds %>%
                rvest::html_table() %>%
                {.[[grep("Totals", .)[1]]]} %>%
                dplyr::filter(.data$Region == "Totals") %>%
                dplyr::pull(.data$Resolved) %>%
                readr::parse_number()
              deceased <- ds %>%
                rvest::html_table() %>%
                {.[[grep("Deceased", .)[1]]]} %>%
                dplyr::pull(.data[["\u200BDeceased"]]) %>% # note zero-width space
                as.character() %>%
                readr::parse_number()
              data.frame(value = resolved - deceased) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Sudbury
    "4b9c88a2-9487-4632-adc5-cfd4a2fddb3f" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Cases1 | Cas1", .)[1]]]} %>%
                dplyr::filter(.[1] == "Cases1 | Cas1") %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Deceased | D\u00E9c\u00E8s", .)[1]]]} %>%
                dplyr::filter(.[1] == "Deceased | D\u00E9c\u00E8s") %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              resolved <- ds %>%
                rvest::html_table() %>%
                {.[[grep("Resolved cases2 | Cas r\u00E9gl\u00E9s2", .)[1]]]} %>%
                dplyr::filter(.[1] == "Resolved cases2 | Cas r\u00E9gl\u00E9s2") %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number()
              deceased <- ds %>%
                rvest::html_table() %>%
                {.[[grep("Deceased | D\u00E9c\u00E8s", .)[1]]]} %>%
                dplyr::filter(.[1] == "Deceased | D\u00E9c\u00E8s") %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number()
              data.frame(value = resolved - deceased) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
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
