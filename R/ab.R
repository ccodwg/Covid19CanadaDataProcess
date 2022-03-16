#' Functions to process datasets: AB
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_ab <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {
  # set defaults
  prov <- "AB"

  # process datasets
  switch(
    uuid,
    "59da1de8-3b4e-429a-9e18-b67ba3834002" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              # calculate number of cases with unknown zone
              n_unknown <- ds %>%
                dplyr::filter(.data$Alberta.Health.Services.Zone == "Unknown") %>% nrow
              ds %>%
                dplyr::select(.data$Alberta.Health.Services.Zone, .data$Date.reported) %>%
                dplyr::count(.data$Alberta.Health.Services.Zone, .data$Date.reported) %>%
                dplyr::mutate(Date.reported = as.Date(.data$Date.reported)) %>%
                # unknown cases should only be reported for the most recent date
                # should be zero for all other dates
                dplyr::mutate(
                  n = dplyr::case_when(
                    .data$Alberta.Health.Services.Zone == "Unknown" &
                      .data$Date.reported == max(.data$Date.reported) ~ n_unknown,
                    .data$Alberta.Health.Services.Zone == "Unknown" ~ as.integer(0),
                    TRUE ~ .data$n
                  )) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  date = .data$Date.reported,
                  value = .data$n
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$Case.status == "Died") %>%
                dplyr::count(.data$Alberta.Health.Services.Zone) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  value = .data$n
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
              ds %>%
                dplyr::filter(.data$Case.status == "Recovered") %>%
                dplyr::count(.data$Alberta.Health.Services.Zone) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "ec1acea4-8b85-4c04-b905-f075de040493" = {
      switch(
        val,
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("li") %>%
                grep("Recovered cases", ., value = TRUE) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                dplyr::mutate(
                  name = val,
                  province = prov,
                  date = date_current,
                  value = as.integer(.data$value)
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
                  rvest::html_elements("li") %>%
                  grep("Total tests completed", ., value = TRUE) %>%
                  readr::parse_number() %>%
                  data.frame(value = .) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_elements("li") %>%
                  grep("People tested", ., value = TRUE) %>%
                  readr::parse_number() %>%
                  data.frame(value = .) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "24a572ea-0de3-4f83-b9b7-8764ea203eb6" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_element("#geospatial") %>%
                rvest::html_table() %>%
                dplyr::select(.data$Zone, .data$Count) %>%
                dplyr::filter(.data$Zone != "All") %>%
                dplyr::mutate(Count = readr::parse_number(.data$Count)) %>%
                dplyr::rename(
                  sub_region_1 = .data$Zone,
                  value = .data$Count
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
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
                {.[[grep("Dose 1.*Dose 2.*Total administered", .)]]} %>%
                rvest::html_table() %>%
                # rename first column
                dplyr::rename("Provider" = 1) %>%
                # filter to total
                dplyr::filter(.data$Provider == "Total") %>%
                # select total administered
                dplyr::select(.data[["Total administered"]]) %>%
                as.character() %>%
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
                rvest::html_elements("table") %>%
                {.[[grep("Dose 1.*Dose 2.*Total administered", .)]]} %>%
                rvest::html_table() %>%
                # rename first column
                dplyr::rename("Provider" = 1) %>%
                # filter to total
                dplyr::filter(.data$Provider == "Total") %>%
                # select second doses
                dplyr::select(.data[["Dose 2"]]) %>%
                as.character() %>%
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
                rvest::html_elements("table") %>%
                {.[[grep("Dose 1.*Dose 2.*Total administered", .)]]} %>%
                rvest::html_table() %>%
                # rename first column
                dplyr::rename("Provider" = 1) %>%
                # filter to total
                dplyr::filter(.data$Provider == "Total") %>%
                # select additional doses
                dplyr::select(dplyr::matches(c("Additional dose", "Dose 3"))) %>%
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
    "d3b170a7-bb86-4bb0-b362-2adc5e6438c2" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                # filter to zone info
                dplyr::slice(-c(1, 2)) %>%
                dplyr::transmute(
                  sub_region_1 = .data$Location,
                  value = readr::parse_number(.data$`Confirmed\n\t\t\tcases`)
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
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                # filter to zone info
                dplyr::slice(-c(1, 2)) %>%
                dplyr::transmute(
                  sub_region_1 = .data$Location,
                  value = readr::parse_number(.data$Deaths)
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "d3b170a7-bb86-4bb0-b362-2adc5e6438c2" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(!grepl("In", .$Location)) %>%
                dplyr::select(.data$Location, .data$`In\n\t\t\thospital**`) %>%
                dplyr::transmute(sub_region_1 = .data$Location,
                                 value = readr::parse_number(.data$`In\n\t\t\thospital**`)) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(!grepl("In", .$Location)) %>%
                dplyr::select(.data$Location, .data$`In intensive\n\t\t\tcare***`) %>%
                dplyr::transmute(sub_region_1 = .data$Location,
                                 value = readr::parse_number(.data$`In intensive\n\t\t\tcare***`)) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "3ced816d-8524-4875-bd69-61fb5603b596" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = .data$Date.reported.to.Alberta.Health,
                  value = .data$Number.of.cases) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        "active" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = .data$Date.reported.to.Alberta.Health,
                  value = .data$Active.cases) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = .data$Date.reported.to.Alberta.Health,
                  value = .data$Number.of.deaths) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = .data$Date.reported.to.Alberta.Health,
                  value = .data$Currently.hospitalized) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = .data$Date.reported.to.Alberta.Health,
                  value = .data$Currently.in.ICU) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
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
