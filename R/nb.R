#' Functions to process datasets: NB
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nb <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "NB"

  # definitions
  nb_zones <- c(
    "Zone 1: South East Region (Moncton)",
    "Zone 2: South Central Region (Saint John)",
    "Zone 3: Central West Region (Fredericton)",
    "Zone 4: North West Region (Edmundston)",
    "Zone 5: North Central Region (Campbellton)",
    "Zone 6: North East Region (Bathurst)",
    "Zone 7: Central East Region (Miramichi)"
  )

  # process datasets
  switch(
    uuid,
    "4f194e3b-39fd-4fe0-b420-8cefa9001f7e" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HealthZnEng %in% nb_zones) %>%
                dplyr::select(.data$HealthZnEng, .data$TotalCases) %>%
                dplyr::rename(
                  sub_region_1 = .data$HealthZnEng,
                  value = .data$TotalCases
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
                dplyr::filter(.data$HealthZnEng %in% nb_zones) %>%
                dplyr::select(.data$HealthZnEng, .data$Deaths) %>%
                dplyr::rename(
                  sub_region_1 = .data$HealthZnEng,
                  value = .data$Deaths
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HealthZnEng == "New Brunswick") %>%
                dplyr::select(.data$Recovered) %>%
                dplyr::rename(value = .data$Recovered) %>%
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
                dplyr::filter(.data$HealthZnEng == "New Brunswick") %>%
                dplyr::select(.data$TotalTests) %>%
                dplyr::rename(value = .data$TotalTests) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "6996a762-56cc-4a27-8e77-1c8734752793" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$TotalReceived) %>%
                dplyr::rename(value = .data$TotalReceived) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_total_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$TotalAdmin) %>%
                dplyr::rename(value = .data$TotalAdmin) %>%
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
                dplyr::select(.data$PopSecondDose) %>%
                dplyr::mutate(value = .data$PopSecondDose) %>%
                dplyr::select(.data$value) %>%
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
                dplyr::select(.data$PopBoosterDose) %>%
                dplyr::rename(value = .data$PopBoosterDose) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "4cf063fa-1bca-409c-867c-f710ef9d17e5" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::select(.data$Date, .data$Hospitalizations) %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$Date / 1000, origin = "1970-01-01"),
                                       tz = "America/Halifax")),
                  value = .data$Hospitalizations) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_ts" = {
              ds$features$attributes %>%
                dplyr::select(.data$Date, .data$ICU) %>%
                dplyr::transmute(
                  date = lubridate::date(
                    lubridate::with_tz(as.POSIXct(.data$Date / 1000, origin = "1970-01-01"),
                                       tz = "America/Halifax")),
                  value = .data$ICU) %>%
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
