#' Functions to process datasets: NU
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nu <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "NU"

  # process datasets
  switch(
    uuid,
    "04ab3773-f535-42ad-8ee4-4d584ec23523" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Total confirmed cases", .)[1]]]} %>%
                dplyr::select(.data$`Total confirmed cases`) %>%
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
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Deaths", .)[1]]]} %>%
                dplyr::select(.data$Deaths) %>%
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
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Total recovered cases", .)[1]]]} %>%
                dplyr::select(.data$`Total recovered cases`) %>%
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
        "testing" = {
          match.arg(testing_type, c("n_tests_completed", "n_people_tested"), several.ok = FALSE)
          switch(
            fmt,
            "prov_cum_current" = {
              if (testing_type == "n_tests_completed") {
                ds %>%
                  rvest::html_table(header = TRUE) %>%
                  {.[[grep("Total tests", .)[1]]]} %>%
                  dplyr::select(.data$`Total tests`) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_table(header = TRUE) %>%
                  {.[[grep("Tests Negative", .)[1]]]} %>%
                  dplyr::filter(.data$Community == "Total") %>%
                  dplyr::select(
                    .data$`Tests Positive`,
                    .data$`Tests Negative`
                    ) %>%
                  dplyr::mutate(
                    `Tests Positive` = readr::parse_number(
                      as.character(.data$`Tests Positive`)), # in case someone adds a special character, e.g. "*"
                    `Tests Negative` = readr::parse_number(
                      as.character(.data$`Tests Negative`))
                  ) %>%
                  dplyr::transmute(value = .data$`Tests Positive` + .data$`Tests Negative`) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              }
            },
            e_fmt()
          )
        },
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Total persons vaccinated with at least one dose in Nunavut", .)[1]]]} %>%
                dplyr::select(
                  .data$`Total persons vaccinated with at least one dose in Nunavut`,
                  .data$`Total persons vaccinated with two doses in Nunavut`
                  ) %>%
                dplyr::transmute(
                  dose_1 = readr::parse_number(as.character(.data$`Total persons vaccinated with at least one dose in Nunavut`)),
                  dose_2 = readr::parse_number(as.character(.data$`Total persons vaccinated with two doses in Nunavut`))
                ) %>%
                dplyr::transmute(
                  value = .data$dose_1 + .data$dose_2
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
               rvest::html_table(header = TRUE) %>%
               {.[[grep("Total persons vaccinated with two doses in Nunavut", .)[1]]]} %>%
               dplyr::select(.data$`Total persons vaccinated with two doses in Nunavut`) %>%
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
        e_val()
      )
    },
    e_uuid()
  )
}
