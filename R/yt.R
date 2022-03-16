#' Functions to process datasets: YT
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_yt <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "YT"

  # process datasets
  switch(
    uuid,
    "4cdeff57-3cbd-4d58-b0a9-c66d8c0c197e" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                {.[[grep("Confirmed Yukon resident cases", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "Confirmed Yukon resident cases") %>%
                dplyr::select(2) %>%
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
                rvest::html_elements("table") %>%
                {.[[grep("Confirmed Yukon resident cases", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "Deceased") %>%
                dplyr::select(2) %>%
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
                rvest::html_elements("table") %>%
                {.[[grep("Confirmed Yukon resident cases", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "Recovered") %>%
                dplyr::select(2) %>%
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
                  rvest::html_elements("table") %>%
                  {.[[grep("Total number of tests", .)[1]]]} %>%
                  rvest::html_table(header = FALSE) %>%
                  dplyr::filter(.data$X1 == "Total number of tests") %>%
                  dplyr::select(2) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_elements("table") %>%
                  {.[[grep("Total number of tests", .)[1]]]} %>%
                  rvest::html_table(header = FALSE) %>%
                  dplyr::filter(.data$X1 == "Total people tested") %>%
                  dplyr::select(2) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              }
            },
            e_fmt()
          )
        },
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                {.[[grep("Total % vaccinated", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "Total doses") %>%
                dplyr::select(2) %>%
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
        "vaccine_dose_2" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                {.[[grep("Total % vaccinated", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "2nd shot") %>%
                dplyr::select(2) %>%
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
        "vaccine_additional_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                {.[[grep("Total % vaccinated", .)[1]]]} %>%
                rvest::html_table(header = FALSE) %>%
                dplyr::filter(.data$X1 == "3rd shot") %>%
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
    "9c29c29f-fd38-45d5-a471-9bfb95abb683" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Metric == "Confirmed Yukon Resident Cases Total") %>%
                {data.frame(value = readr::parse_number(as.character(.$number)))} %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Metric == "Confirmed Yukon Resident Cases Deceased") %>%
                {data.frame(value = readr::parse_number(as.character(.$number)))} %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Metric == "Confirmed Yukon Resident Cases Resolved") %>%
                {data.frame(value = readr::parse_number(as.character(.$number)))} %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "active" = {
          switch(
            fmt,
            "prov_cum_current" = {
              # note that total cases seems to be less than active + recovered + deceased
              ds$features$attributes %>%
                dplyr::filter(.data$Metric == "Cases Total Active") %>%
                {data.frame(value = readr::parse_number(as.character(.$number)))} %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "4a33ab2c-32a4-4630-8f6b-2bac2b1ce7ca" = {
      switch(
        val,
        "testing" = {
          match.arg(testing_type, c("n_tests_completed"), several.ok = FALSE)
          switch(
            fmt,
            "prov_ts" = {
              if (testing_type == "n_tests_completed") {
                ds$features$attributes %>%
                  dplyr::transmute(
                    date = lubridate::date(
                      lubridate::with_tz(as.POSIXct(.data$DATE_RESULTED / 1000, origin = "1970-01-01"),
                                         tz = "UTC")),
                    value = .data$n_tests) %>%
                  helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "387473c7-bcb9-4712-82fb-cd0355793cdc" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$ageGroup == "Total") %>%
                dplyr::transmute(value = .data$dose1 + .data$dose2 + .data$dose3) %>%
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
                dplyr::filter(.data$ageGroup == "Total") %>%
                dplyr::transmute(value = .data$dose1) %>%
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
                dplyr::filter(.data$ageGroup == "Total") %>%
                dplyr::transmute(value = .data$dose2) %>%
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
                dplyr::filter(.data$ageGroup == "Total") %>%
                dplyr::transmute(value = .data$dose3) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "8eb9a22f-a2c0-4bdb-8f6c-ef8134901efe" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$REPORT_DATE_RECEIVED / 1000, origin = "1970-01-01"),
                                       tz = "UTC")),
                  value = .data$DAILY_CASES) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "2fb9f970-04e0-4502-ae4a-67ec866dcad9" = {
      switch(
        val,
        "hospitalizations_cum" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$HOSPITAL_ADMISSION_DATE5 / 1000, origin = "1970-01-01"),
                                       tz = "UTC")),
                  value = .data$TOTAL_HOSPITALIZATIONS) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "7698e84c-f43f-4a83-828a-1d26e21467b9" = {
      switch(
        val,
        # THE CASES BY VACCINATION STATUS CHART ARE UPDATED WEEKLY ON MONDAYS (EXCLUDING HOLIDAYS). THE DATE SHOWN IS THE LAST DATE PRIOR TO REPORTING (i.e. SUNDAY). DETAILED DEFINITIONS AT: https://covid-19-data-dashboard.service.yukon.ca/
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$EPI_WEEK_DATE / 1000, origin = "1970-01-01"), tz = "UTC")),
                  date = date - 7,
                  value = .data$TOTAL_CASES) %>%
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
