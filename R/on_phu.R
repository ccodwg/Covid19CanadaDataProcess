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
    e_uuid()
  )
}
