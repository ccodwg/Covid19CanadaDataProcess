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
                {.[[grep("Confirmed cases", .)[1]]]} %>%
                dplyr::rename("column_1" = 1) %>% # avoid zero-length variable name error in some circumstances
                {dplyr::filter(., .[[1]] == "Confirmed cases")} %>%
                dplyr::select(.data$Current) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                dplyr::rename("column_1" = 1) %>% # avoid zero-length variable name error in some circumstances
                {dplyr::filter(., .[[1]] == "Deceased")} %>%
                dplyr::select(.data$Current) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {.[[grep("Resolved", .)[1]]]} %>%
                dplyr::rename("column_1" = 1) %>% # avoid zero-length variable name error in some circumstances
                {dplyr::filter(., .[[1]] == "Deceased" | .[[1]] == "Resolved")} %>%
                # rename first column so it is easier to reference
                dplyr::rename("Name" = 1) %>%
                dplyr::select(.data$Name, .data$Current) %>%
                dplyr::mutate(Current = readr::parse_number(as.character(.data$Current))) %>%
                {.[.$Name == "Resolved", "Current"] - .[.$Name == "Deceased", "Current"]} %>%
                dplyr::pull() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Brant
    "2e7a5549-92ae-473d-a97a-7b8e0c1ddbbc" = {
      hr <- "Brant"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("Total confirmed cases", .)[1]]]} %>%
                stringr::str_replace_all(., "\\,", "") %>% # Remove commas
                stringr::str_extract(., "^([^.]+)\\.([^.]+)\\.*") %>% # Keeps everything before the second period
                stringr::str_replace_all(., "\\.", "") %>% # Remove periods or else readr::parse_number() throws a parsing error
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("Deaths", .)[1]]]} %>%
                stringr::str_replace_all(., "\\,", "") %>% # Remove commas
                stringr::str_extract(., "^([^.]+)\\.([^.]+)\\.*") %>% # Keeps everything before the second period
                stringr::str_replace_all(., "\\.", "") %>% # Remove periods or else readr::parse_number() throws a parsing error
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("Total resolved cases", .)[1]]]} %>%
                stringr::str_replace_all(., "\\,", "") %>% # Remove commas
                stringr::str_extract(., "^([^.]+)\\.([^.]+)\\.*") %>% # Keeps everything before the second period
                stringr::str_replace_all(., "\\.", "") %>% # Remove periods or else readr::parse_number() throws a parsing error
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Chatham-Kent
    "fe08035c-2c03-4960-a642-bde1fe18c857" = {
      hr <- "Chatham-Kent"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[grep("Cases", .)]} %>%
                `[`(2) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[c(grep("Total COVID Deaths", .), grep("Total COVID Deaths", .)+1)]} %>% # Extract value in the adjacent vector element to the value label
                stringr::str_c(collapse = " ") %>%
                stringr::str_replace_all(., "\\.", "") %>% # Need to remove periods or it throws a parsing error %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        # "recovered" = {
        #   switch(
        #     fmt,
        #     "hr_cum_current" = {
        #       ds %>%
        #         rvest::html_elements(".card") %>%
        #         rvest::html_attr("aria-label") %>%
        #         {.[c(grep("Total Resolved Cases", .), grep("Total Resolved Cases", .)+1)]} %>% # Extract value in the adjacent vector element to the value label
        #         stringr::str_c(collapse = " ") %>%
        #         stringr::str_replace_all(., "\\.", "") %>% # Need to remove periods or it throws a parsing error %>%
        #         readr::parse_number() %>%
        #         data.frame(value = .) %>%
        #         helper_cum_current(loc = "hr", val, prov, date_current, hr)
        #     },
        #     e_fmt()
        #   )
        # },
        e_val()
      )
    },
    # Durham
    "ba7b0d74-5fe2-41d8-aadb-6320ff9acb21" = {
      hr <- "Durham"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements("div") %>%
                {.[grep("Total Confirmed Cases", rvest::html_attr(., "aria-label"))][1]} %>%
                rvest::html_element(".card") %>%
                rvest::html_attr("aria-label") %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements("div") %>%
                {.[grep("Total Deceased\\*", rvest::html_attr(., "aria-label"))][1]} %>%
                rvest::html_element(".card") %>%
                rvest::html_attr("aria-label") %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements("div") %>%
                {.[grep("Total Resolved", rvest::html_attr(., "aria-label"))][1]} %>%
                rvest::html_element(".card") %>%
                rvest::html_attr("aria-label") %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
              ds %>%
                rvest::html_element(".stats") %>%
                rvest::html_elements(".card") %>%
                {.[[grep("Total Cumulative Cases", .)[1]]]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_element(".stats") %>%
                rvest::html_elements(".card") %>%
                {.[[grep("Total Deceased", .)[1]]]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_element(".stats") %>%
                rvest::html_elements(".card") %>%
                {.[[grep("Total Resolved", .)[1]]]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("confirmed cases", .)[1]]]} %>%
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
                rvest::html_elements(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("death", .)[1]]]} %>%
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
                rvest::html_elements(".col-sm-7 p") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("resolved cases", .)[1]]]} %>%
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
    # Haldimand-Norfolk
    "07fcf6b9-6e61-433e-b1a8-a951ee15b01d" = {
      hr <- "Haldimand-Norfolk"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".visualContainer") %>%
                {.[grep("All Positive Cases", .)][1]} %>%
                rvest::html_elements(".card") %>%
                {.[grep("Count of Result", .)][1]} %>%
                rvest::html_attr("aria-label") %>%
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
                rvest::html_elements(".visualContainer") %>%
                rvest::html_elements(".card") %>%
                {.[grep("Count of Case Status 2", .)][2]} %>%
                rvest::html_attr("aria-label") %>%
                sub("Count of Case Status 2", "", .) %>%
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
              recovered <- ds %>%
                rvest::html_elements(".visualContainer") %>%
                rvest::html_elements(".card") %>%
                {.[grep("Count of Cases Status 1", .)][1]} %>%
                rvest::html_attr("aria-label") %>%
                sub("Count of Cases Status 1", "", .) %>%
                readr::parse_number()
              non_covid_deaths <- ds %>%
                rvest::html_elements(".visualContainer") %>%
                rvest::html_elements(".card") %>%
                {.[grep("Count of Case Status 2", .)][3]} %>% # first two are active cases and mortality
                rvest::html_attr("aria-label") %>%
                sub("Count of Case Status 2", "", .) %>%
                readr::parse_number()
                data.frame(
                  value = recovered + non_covid_deaths
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Haliburton Kawartha Pineridge
    "c1cd96db-69c3-4970-9a4b-e7bcdc12d39b" = {
      hr <- "Haliburton Kawartha Pineridge"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              confirmed_cases <- ds %>%
                rvest::html_elements(".bodyCells") %>%
                `[`(2) %>% # second table
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(4) %>% # Confirmed Cases (Total to date)
                rvest::html_children() %>%
                `[`(5) %>% # Total
                rvest::html_text2() %>%
                readr::parse_number()
              probable_deaths <- ds %>%
                rvest::html_elements(".bodyCells") %>%
                `[`(2) %>% # second table
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(8) %>% # Probable Deaths (Total to date)
                rvest::html_children() %>%
                `[`(5) %>% # Total
                rvest::html_text2() %>%
                readr::parse_number()
              data.frame(value = confirmed_cases + probable_deaths) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              confirmed_deaths <- ds %>%
                rvest::html_elements(".bodyCells") %>%
                `[`(2) %>% # second table
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(7) %>% # Confirmed Deaths (Total to date)
                rvest::html_children() %>%
                `[`(5) %>% # Total
                rvest::html_text2() %>%
                readr::parse_number()
              probable_deaths <- ds %>%
                rvest::html_elements(".bodyCells") %>%
                `[`(2) %>% # second table
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(8) %>% # Probable Deaths (Total to date)
                rvest::html_children() %>%
                `[`(5) %>% # Total
                rvest::html_text2() %>%
                readr::parse_number()
              data.frame(value = confirmed_deaths + probable_deaths) %>%
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
                rvest::html_elements(".bodyCells") %>%
                `[`(2) %>% # second table
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(5) %>% # Confirmed Cases Resolved (Total to date)
                rvest::html_children() %>%
                `[`(5) %>% # Total
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Halton
    "8d4067a7-4828-4b09-8396-089231cf2e94" = {
      hr <- "Halton"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                {.[grep("ProbConf", .)][1]} %>%
                rvest::html_text2() %>%
                stringr::str_extract("(\\d+|\\d{1,3}(,\\d{3})*)(\\.\\d+)? were confirmed cases") %>%
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
                rvest::html_element(".bodyCells") %>%
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(7) %>%
                rvest::html_text2() %>%
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
                rvest::html_element(".bodyCells") %>%
                rvest::html_element("div") %>%
                rvest::html_element("div") %>%
                rvest::html_children() %>%
                `[`(5) %>%
                rvest::html_text2() %>%
                readr::parse_number()
              probable_cases <- ds %>%
                rvest::html_elements(".card") %>%
                {.[grep("ProbConf", .)][1]} %>%
                rvest::html_text2() %>%
                stringr::str_extract("(\\d+|\\d{1,3}(,\\d{3})*)(\\.\\d+)? were probable cases") %>%
                readr::parse_number()
              data.frame(
                value = resolved - probable_cases
              ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Hamilton
    "b8ef690e-d23f-4b7d-8cf8-bc4a0f3d0a84" = {
      hr <- "Hamilton"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("TotalCases", .)[1]]]} %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("FatalCount", .)[1]]]} %>%
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
              # ignore "resolved %"
              cases <- ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("TotalCases", .)[1]]]} %>%
                readr::parse_number()
              mortalities <- ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("FatalCount", .)[1]]]} %>%
                readr::parse_number()
              active_cases <- ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[[grep("ActiveCount", .)[1]]]} %>%
                readr::parse_number()
              data.frame(
                value = cases - mortalities - active_cases
              ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Hastings Prince Edward
    "22c51a16-63d6-470c-8b8a-54ae243883b5" = {
      hr <- "Hastings Prince Edward"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[grep("Total", .)][1]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Kingston Frontenac Lennox & Addington
    "83d1fa13-7fb3-4079-b3dc-5bc50c584fd3" = {
      hr <- "Kingston Frontenac Lennox & Addington"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[grep("# of Cases", .)][1]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[grep("# of Deaths", .)][1]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>%
                {.[grep("# of Cases Resolved", .)][1]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Lambton
    "8d0cf226-b9b7-4fc3-8100-a4f56dec6792" = {
      hr <- "Lambton"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".textRun") %>%
                {.[8]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".textRun") %>%
                {.[14]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".textRun") %>%
                {.[12]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Middlesex-London
    "b32a2f6b-7745-4bb1-9f9b-7ad0000d98a0" = {
      hr <- "Middlesex-London"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".textbox") %>%
                `[`(4) %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".textbox") %>%
                `[`(10) %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".textbox") %>%
                `[`(12) %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Niagara
    "e1887eb2-538f-4610-bc00-bcd7d929a375" = {
      hr <- "Niagara"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("CasesCount", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Number of deaths", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Count_RecoveredCases", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # North Bay
    "3178dd11-17af-4478-a72e-e1a35d7d1b2d" = {
      hr <- "North Bay Parry Sound"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Count of Age Group", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Deaths", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Resolved", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {.[unlist(lapply(., nrow)) != 0]} %>%
                {.[[grep("TOTALS", .)[1]]]} %>%
                dplyr::filter(.data$`Local Health Hub` == "TOTALS") %>%
                dplyr::pull(.data$Total) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {.[unlist(lapply(., nrow)) != 0]} %>%
                {.[[grep("Deceased", .)[1]]]} %>%
                dplyr::pull(.data[["\u200BDeceased"]]) %>% # note zero-width space
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {.[unlist(lapply(., nrow)) != 0]} %>%
                {.[[grep("TOTALS", .)[1]]]} %>%
                dplyr::filter(.data$`Local Health Hub` == "TOTALS") %>%
                dplyr::pull(.data$Resolved) %>%
                as.character() %>%
                readr::parse_number()
              deceased <- ds %>%
                rvest::html_table() %>%
                {.[unlist(lapply(., nrow)) != 0]} %>%
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
    # Peel
    "34b7dda2-1843-47e1-9c24-0c2a7ab78431" = {
      hr <- "Peel"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Total cases", .)[1]]]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Case Count Recovered1", .)[3]]]} %>%
                gsub("Recovered1","",.) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".card") %>%
                rvest::html_attr("aria-label") %>% {.[[grep("Case Count Recovered1", .)[1]]]} %>%
                gsub("Recovered1","",.) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements("p") %>%
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
                rvest::html_elements("p") %>%
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
                rvest::html_elements("p") %>%
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
      hr <- "Porcupine"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements("li") %>%
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
                rvest::html_elements("li") %>%
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
                rvest::html_elements("li") %>%
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
    # Renfrew
    "688bf944-9be6-49c3-ae5d-848ae32bad92" = {
      hr <- "Renfrew"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements("div.col-md-6") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Confirmed Cases", .)[1]]]} %>%
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
                rvest::html_elements("div.col-md-6") %>%
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
                rvest::html_elements("div.col-md-6") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Resolved", .)[1]]]} %>%
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
    # Simcoe Muskoka
    "7106106a-2f43-4ed2-b2a2-a75a7046ff81" = {
      hr <- "Simcoe Muskoka"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>% rvest::html_table() %>% {.[[1]]} %>%
                dplyr::filter(.$X1=="Total # of Confirmed Cases") %>%
                {.[2]} %>% as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>% rvest::html_table() %>% {.[[1]]} %>%
                dplyr::filter(.$X1=="Total # of Deaths") %>%
                {.[2]} %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>% rvest::html_table() %>% {.[[1]]} %>%
                dplyr::filter(.$X1=="Total # of Resolved Cases") %>%
                {.[2]} %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
      hr <- "Sudbury"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                {.[[grep("Cases1", .)[1]]]} %>%
                {dplyr::filter(., grepl("Cases1", .[[1]]))} %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {dplyr::filter(., grepl("Deceased", .[[1]]))} %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                {.[[grep("Resolved cases", .)[1]]]} %>%
                {dplyr::filter(., grepl("Resolved cases", .[[1]]))} %>%
                dplyr::pull(.data$`Current / Actuellement`) %>%
                sub(" ", "", .) %>%
                readr::parse_number()
              deceased <- ds %>%
                rvest::html_table() %>%
                {.[[grep("Deceased", .)[1]]]} %>%
                {dplyr::filter(., grepl("Deceased", .[[1]]))} %>%
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
    # Thunder Bay
    "942e48c4-1148-46e1-a5d3-e25aa9bede05" = {
      hr <- "Thunder Bay"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".InfographicEditor-Contents-Item") %>%
                {.[grep("^Cumulative Confirmed Cases", rvest::html_text2(.))]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".InfographicEditor-Contents-Item") %>%
                {.[grep("^Deceased", rvest::html_text2(.))]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".InfographicEditor-Contents-Item") %>%
                {.[grep("^Resolved Cases", rvest::html_text2(.))]} %>%
                rvest::html_text2() %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Timiskaming
    "9c7bbba4-33ba-493a-8ea1-4eedd5149bc0" = {
      hr <- "Timiskaming"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::filter(.data$X1 == "Positive cases") %>%
                {sum(readr::parse_number(as.character(.[1, -1])))} %>%
                data.frame(value = .) %>%
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
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::filter(.data$X1 == "Deaths") %>%
                {sum(readr::parse_number(as.character(.[1, -1])))} %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              total_cases <- ds %>%
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::filter(.data$X1 == "Positive cases") %>%
                {sum(readr::parse_number(as.character(.[1, -1])))}
              total_deaths <- ds %>%
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::filter(.data$X1 == "Deaths") %>%
                {sum(readr::parse_number(as.character(.[1, -1])))}
              total_active <- ds %>%
                rvest::html_table(header = FALSE) %>%
                `[[`(1) %>%
                dplyr::filter(grepl("Active cases", .data$X1)) %>%
                {sum(readr::parse_number(as.character(.[1, -1])))}
              data.frame(value = total_cases - total_deaths - total_active) %>%
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
      hr <- "Toronto"
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
    # Wellington Dufferin Guelph
    "e00e2148-b0ea-458b-9f00-3533e0c5ae8e" = {
      hr <- "Wellington Dufferin Guelph"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".content") %>%
                {.[grep("Total Confirmed", .)][1]} %>%
                rvest::html_text2() %>%
                stringr::str_split("\u2503") %>%
                `[[`(1) %>%
                {.[grep("Total Confirmed", .)]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".content") %>%
                {.[grep("Fatal Cases", .)][1]} %>%
                rvest::html_text2() %>%
                stringr::str_split("\u2503") %>%
                `[[`(1) %>%
                {.[grep("Fatal Cases", .)]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
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
                rvest::html_elements(".content") %>%
                {.[grep("Total Resolved", .)][1]} %>%
                rvest::html_text2() %>%
                stringr::str_split("\u2503") %>%
                `[[`(1) %>%
                {.[grep("Total Resolved", .)]} %>%
                readr::parse_number() %>%
                data.frame(value = .) %>%
                helper_cum_current(loc = "hr", val, prov, date_current, hr)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Windsor-Essex
    "01574538-062f-4a41-9dd5-8fdb72a0fe03" = {
      hr <- "Windsor-Essex"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                rvest::html_elements(".text-center") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Confirmed Cases", .)[1]]]} %>%
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
                rvest::html_elements(".text-center") %>%
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
                rvest::html_elements(".text-center") %>%
                rvest::html_text(trim = TRUE) %>%
                {.[[grep("Resolved Cases", .)[1]]]} %>%
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
    # York
    "3821cc66-f88d-4f12-99ca-d36d368872cd" = {
      hr <- "York"
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::summarise(value = dplyr::n()) %>%
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
                dplyr::filter(.data$Status == "Deceased") %>%
                dplyr::summarise(value = dplyr::n()) %>%
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
                dplyr::filter(.data$Status == "Resolved") %>%
                dplyr::summarise(value = dplyr::n()) %>%
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
