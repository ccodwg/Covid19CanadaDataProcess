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
              ds %>%
                dplyr::transmute(
                  sub_region_1 = dplyr::case_when(
                    .data$Alberta.Health.Services.Zone == "" ~ "Unknown",
                    is.na(.data$Alberta.Health.Services.Zone) ~ "Unknown",
                    TRUE ~ .data$Alberta.Health.Services.Zone
                  ),
                  date = as.Date(.data$Date.reported)) %>%
                dplyr::count(.data$sub_region_1, .data$date, name = "value") %>%
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
                dplyr::filter(.data$Death.status == "Died") %>%
                dplyr::transmute(
                  sub_region_1 = dplyr::case_when(
                    .data$Alberta.Health.Services.Zone == "" ~ "Unknown",
                    is.na(.data$Alberta.Health.Services.Zone) ~ "Unknown",
                    TRUE ~ .data$Alberta.Health.Services.Zone
                  )) %>%
                dplyr::count(.data$sub_region_1, name = "value") %>%
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
        "testing" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                rvest::html_elements("script") %>%
                {.[which(rvest::html_attr(., "data-for") == "htmlwidget-1c2f041cb39c009c0b5e")]} %>%
                rvest::html_text2() %>%
                jsonlite::fromJSON() %>%
                {
                  data.frame(
                    date = as.Date(unlist(.$x$data$x)),
                    value = as.integer(unlist(.$x$data$y))
                  )
                } %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
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
                  value = readr::parse_number(.data$`Confirmed cases`)
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
