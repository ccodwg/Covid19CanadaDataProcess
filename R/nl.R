#' Functions to process datasets: NL
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nl <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "NL"

  # process datasets
  switch(
    uuid,
    "f0e10f54-a4db-48d8-9c4e-8571e663ca28" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_of_cases) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_of_cases
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
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_of_deaths) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_of_deaths
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
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_recovered) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_recovered
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_people_tested) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_people_tested
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "6d1b0f2e-0ac0-4ad8-a383-619065ec5a52" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$DosesReceived) %>%
                dplyr::rename(value = .data$DosesReceived) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "64d10e59-6b60-474f-9a4f-6c6a2c71b1a8" = {
      switch(
        val,
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(value = .data$Administered) %>%
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
                # Vaccinated = 12+ / Vaccinated5to11 = 5-11
                dplyr::transmute(value = .data$Vaccinated + .data$Vaccinated5to11) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_additional_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  value = .data$Administered -
                    .data$OneDose - .data$OneDose5to11 -
                    .data$Vaccinated - .data$Vaccinated5to11) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "8419f6f1-b80b-4247-84e5-6414ab0154d8" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$total_cases) %>%
                dplyr::rename(value = .data$total_cases) %>%
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
                dplyr::select(.data$total_deaths) %>%
                dplyr::rename(value = .data$total_deaths) %>%
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
                dplyr::select(.data$total_recovered) %>%
                dplyr::rename(value = .data$total_recovered) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$total_tests_delivered) %>%
                dplyr::rename(value = .data$total_tests_delivered) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "13a35524-89bf-46a2-afc5-d88ef81bfa82" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::select(.data$date_of_update, .data$currently_hospitalized) %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$date_of_update / 1000, origin = "1970-01-01"),
                                       tz = "America/St_Johns")),
                  value = .data$currently_hospitalized) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f0e10f54-a4db-48d8-9c4e-8571e663ca28" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  sub_region_1 = .data$name,
                  value = .data$currently_hospitalized
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  sub_region_1 = .data$name,
                  value = .data$current_in_icu
                ) %>%
              helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "34f45670-34ed-415c-86a6-e14d77fcf6db" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::transmute(
                  sub_region_1 = .data$Name,
                  value = .data$Total_Case
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$Deaths
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        "recovered" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$Recover
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        "active" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$Active_Case
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        "testing" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$Tests
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        "hospitalizations" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$Hospital
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        "icu" = {
          ds$features$attributes %>%
            dplyr::transmute(
              sub_region_1 = .data$Name,
              value = .data$ICU
            ) %>%
            helper_cum_current(loc = "hr", val, prov, date_current)
        },
        e_val()
      )
    },
    e_uuid()
  )
}
