#' Functions to process datasets: ON
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_on <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # if a health region is specified, pass off to on_phu
  if (!is.null(hr)) {
    return(process_on_phu(uuid, val, fmt, ds,
                          prov, hr, date_current, testing_type))
  }

  # set defaults
  prov <- "ON"

  # process datasets
  switch(
    uuid,
    "921649fa-c6c0-43af-a112-23760da4d622" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(.data$Reporting_PHU, .data$Case_Reported_Date) %>%
                dplyr::count(.data$Reporting_PHU, .data$Case_Reported_Date) %>%
                dplyr::mutate(Case_Reported_Date = as.Date(.data$Case_Reported_Date)) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
                  date = .data$Case_Reported_Date,
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
                dplyr::filter(.data$Outcome1 == "Fatal") %>%
                dplyr::select(.data$Reporting_PHU) %>%
                dplyr::count(.data$Reporting_PHU) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
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
                dplyr::filter(.data$Outcome1 == "Resolved") %>%
                dplyr::select(.data$Reporting_PHU) %>%
                dplyr::count(.data$Reporting_PHU) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
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
    "75dd0545-f728-4af5-8185-4269596785ef" = {
      switch(
        val,
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$cum_deaths_new_methodology
                ) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "a8b1be1a-561a-47f5-9456-c553ea5b2279" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::select(.data$Total.patients.approved.for.testing.as.of.Reporting.Date) %>%
                # this is called "Tests completed" on the Ontario testing data website
                # https://covid-19.ontario.ca/data/testing-volumes-and-results
                dplyr::rename(value = .data$Total.patients.approved.for.testing.as.of.Reporting.Date) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "170057c4-3231-4f15-9438-2165c5438dda" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::transmute(value = .data$total_doses_administered) %>%
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
                dplyr::slice_tail(n = 1) %>%
                dplyr::transmute(value = .data$total_individuals_fully_vaccinated) %>%
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
                dplyr::slice_tail(n = 1) %>%
                dplyr::transmute(value = .data$total_individuals_3doses) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            }
          )
        },
        e_val()
      )
    },
    "73fffd44-fbad-4de8-8d32-00cc5ae180a6" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(
                  .data$FILE_DATE,
                  .data$PHU_NAME,
                  .data$ACTIVE_CASES,
                  .data$RESOLVED_CASES,
                  .data$DEATHS) %>%
                dplyr::filter(.data$PHU_NAME != "") %>%
                dplyr::rowwise() %>%
                dplyr::mutate(value = sum(.data$ACTIVE_CASES + .data$RESOLVED_CASES + .data$DEATHS)) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(FILE_DATE = as.Date(.data$FILE_DATE)) %>%
                dplyr::rename(
                  date = .data$FILE_DATE,
                  sub_region_1 = .data$PHU_NAME) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$date,
                  .data$value
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
              },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(
                  .data$FILE_DATE,
                  .data$PHU_NAME,
                  .data$DEATHS) %>%
                dplyr::filter(.data$PHU_NAME != "") %>%
                dplyr::mutate(FILE_DATE = as.Date(.data$FILE_DATE)) %>%
                dplyr::rename(
                  date = .data$FILE_DATE,
                  sub_region_1 = .data$PHU_NAME,
                  value = .data$DEATHS) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$date,
                  .data$value
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(
                  .data$FILE_DATE,
                  .data$PHU_NAME,
                  .data$RESOLVED_CASES) %>%
                dplyr::filter(.data$PHU_NAME != "") %>%
                dplyr::mutate(FILE_DATE = as.Date(.data$FILE_DATE)) %>%
                dplyr::rename(
                  date = .data$FILE_DATE,
                  sub_region_1 = .data$PHU_NAME,
                  value = .data$RESOLVED_CASES) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$date,
                  .data$value
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
              },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "4b214c24-8542-4d26-a850-b58fc4ef6a30" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$date <- as.Date(ds$date, format = "%m/%d/%Y")
              ds %>%
                dplyr::select(.data$date, .data$hospitalizations) %>%
                dplyr::group_by(.data$date) %>%
                dplyr::summarize(value = sum(.data$hospitalizations), .groups = "drop") %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$date <- as.Date(ds$date, format = "%m/%d/%Y")
              # drop dates where any region has negative or empty values for icu_former_covid
              ds$icu_former_covid <- as.integer(ds$icu_former_covid)
              bad_dates <- unique(ds[is.na(ds$icu_former_covid), "date", drop = TRUE])
              ds <- ds[!ds$date %in% bad_dates, ]
              # 2023-09-08 also seems to have some weird negative values for icu_former_covid but these get cancelled out
              # by values in icu_current_covid column
              ds %>%
                dplyr::select(.data$date, .data$icu_current_covid, .data$icu_former_covid) %>%
                dplyr::group_by(.data$date) %>%
                dplyr::summarize(value = sum(.data$icu_current_covid + .data$icu_former_covid),
                                 .groups = "drop") %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "a7f839da-8c36-4569-bd99-1a07adc0700a" = {
      # DO NOT USE FOR LATER TIME PERIODS
      # PROCESSING CURRENTLY EXCLUDES THOSE UNDER 12
      switch(
        val,
        "vaccine_administration_dose_1" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(.data$Agegroup == "Ontario_12plus") |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = .data$At.least.one.dose_cumulative
                ) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_2" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(.data$Agegroup == "Ontario_12plus") |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = .data$Second_dose_cumulative
                ) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_3" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(.data$Agegroup == "Ontario_12plus") |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = .data$third_dose_cumulative
                ) |>
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
