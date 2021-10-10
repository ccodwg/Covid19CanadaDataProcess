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
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(header = TRUE) %>%
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
    # Eastern
    "cd1db4e8-c4e5-4b24-86a5-2294281919c6" = {
      hr <- "Eastern"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>% rvest::html_table(header = TRUE) %>%
                {.[[grep("Confirmed Cases", .)[1]]]} %>%
                dplyr::select(.data$`EOHU`) %>%
                `[[`(1, 1) %>%
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
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_nodes("span") %>%
                rvest::html_text(trim = TRUE) %>%
                dplyr::nth(8) %>%
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
                rvest::html_nodes("span") %>%
                rvest::html_text(trim = TRUE) %>%
                dplyr::nth(7) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Grey Bruce
    "eac45a46-e5b5-4e75-9393-77995cd7e219" = {
      hr <- "Grey Bruce"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_nodes(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                dplyr::nth(4) %>%
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
                rvest::html_nodes(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                dplyr::nth(8) %>%
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
                rvest::html_nodes(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                dplyr::nth(7) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
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
    # Ottawa
    "d8d4cbc6-d0a5-4544-ad3e-5a3c3060f973" = {
      hr <- "Ottawa"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::mutate(date = lubridate::ymd(.data$Date)) %>%
                dplyr::filter(.data$date == max(.data$date)) %>%
                dplyr::pull(.data$Cumulative.Cases.by.Episode.Date) %>%
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
                dplyr::mutate(date = lubridate::ymd(.data$Date)) %>%
                dplyr::filter(.data$date == max(.data$date)) %>%
                dplyr::pull(.data$Cumulative.Deaths.by.Date.of.Death) %>%
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
                dplyr::mutate(date = lubridate::ymd(.data$Date)) %>%
                dplyr::filter(.data$date == max(.data$date)) %>%
                dplyr::pull(.data$Cumulative.Resolved.Cases.by.Episode.Date) %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Peterborough
    "c3aa6a5e-2ff5-4158-83ab-dcde251bc365" = {
      hr <- "Peterborough"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_nodes("p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Total confirmed cases", .)[1]]]} %>%
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
                rvest::html_nodes("p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Deaths", .)[1]]]} %>%
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
                rvest::html_nodes("p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Resolved cases", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Porcupine
    "00cc3ae2-7bf8-4074-81b7-8e06e91c947a" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_nodes("li") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Number of Cases", .)[1]]]} %>%
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
                rvest::html_nodes("li") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Deaths", .)[1]]]} %>%
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
                rvest::html_nodes("li") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Recovered", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Toronto
    "ebad185e-9706-44f4-921e-fc89d5cfa334" = {
      switch(
        val,
        # sheet = "Status"
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$`Indicator Name` == "Case Count - Overall") %>%
                dplyr::pull(.data$`Cumulative Count`) %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        # sheet = "Status"
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$`Indicator Name` == "Fatal Cases") %>%
                dplyr::pull(.data$`Cumulative Count`) %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        # sheet = "Status"
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$`Indicator Name` == "Recovered Cases") %>%
                dplyr::pull(.data$`Cumulative Count`) %>%
                data.frame(
                  value = .
                ) %>%
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
