#' Functions to process datasets: BC
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_bc <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "BC"

  # function: process cases for HA time series (derivatives of 4f9dc8b7-7b42-450e-a741-a0f6a621d2af)
  ha_cases_timeseries <- function(ds, val, prov) {
    ds$features$attributes %>%
      dplyr::mutate(Date = lubridate::date(
        lubridate::with_tz(as.POSIXct(.data$Date / 1000, origin = "1970-01-01"),
                           tz = "America/Vancouver"))) %>%
      dplyr::select( .data$Date, .data$HA, .data$Cases_Reported) %>%
      dplyr::arrange(.data$HA, .data$Date) %>%
      dplyr::rename(
        sub_region_1 = .data$HA,
        date = .data$Date,
        value = .data$Cases_Reported
      ) %>%
      helper_ts(loc = "hr", val, prov, convert_to_cum = TRUE)
  }

  # process datasets
  switch(
    uuid,
    "91367e1d-8b79-422c-b314-9b3441ba4f42" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$Cases) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$Cases
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
                dplyr::select(.data$HA_Name, .data$Deaths) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
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
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$Recovered) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$Recovered
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "active" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$ActiveCases) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$ActiveCases
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f9a8dea5-1eed-447b-a9a0-be2a4b62d6a6" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::filter(.data$Region == "BC") %>%
                dplyr::pull(.data$New_Tests) %>%
                {data.frame(value = sum(.))} %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "9d940861-0252-4d33-b6e8-23a2eeb105bf" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Tot_Doses_Received) %>%
                sum %>%
                data.frame(
                  value = .
                ) %>%
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
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Tot_Doses_Admin) %>%
                sum %>%
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
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Two_Dose_Admin) %>%
                sum %>%
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
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Other_Doses) %>%
                sum %>%
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
    "ab6abe51-c9b1-4093-b625-93de1ddb6302" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(.data$HA, .data$Reported_Date) %>%
                dplyr::count(.data$HA, .data$Reported_Date) %>%
                dplyr::mutate(Reported_Date = as.Date(.data$Reported_Date)) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA,
                  date = .data$Reported_Date,
                  value = .data$n
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "91367e1d-8b79-422c-b314-9b3441ba4f42" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$CurrentlyHosp) %>%
                dplyr::rename(sub_region_1 = .data$HA_Name,
                              value = .data$CurrentlyHosp) %>%
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
                dplyr::select(.data$HA_Name, .data$CurrentlyICU) %>%
                dplyr::rename(sub_region_1 = .data$HA_Name,
                              value = .data$CurrentlyICU) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "4f9dc8b7-7b42-450e-a741-a0f6a621d2af" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HSDA == "All" & .data$HA != "All") %>%
                dplyr::mutate(Date = lubridate::date(
                  lubridate::with_tz(as.POSIXct(.data$Date / 1000, origin = "1970-01-01"),
                                     tz = "America/Vancouver"))) %>%
                dplyr::select( .data$Date, .data$HA, .data$Cases_Reported) %>%
                dplyr::arrange(.data$HA, .data$Date) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA,
                  date = .data$Date,
                  value = .data$Cases_Reported
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Interior
    "a8637b6c-babf-48cd-aeab-2f38c713f596" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ha_cases_timeseries(ds, val, prov)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Fraser
    "f7cd5492-f23b-45a5-9d9b-118ac2b47529" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ha_cases_timeseries(ds, val, prov)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Vancouver Coastal
    "1ad7ef1b-1b02-4d5c-aec2-4923ea100e97" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ha_cases_timeseries(ds, val, prov)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Vancouver Island
    "89b48da6-bed9-4cd4-824c-8b6d82ffba24" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ha_cases_timeseries(ds, val, prov)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    # Northern
    "def3aca2-3595-4d70-a5d2-d51f78912dda" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ha_cases_timeseries(ds, val, prov)
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
