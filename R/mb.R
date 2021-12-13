#' Functions to process datasets: MB
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_mb <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "MB"

  # definitions
  mb_rha <- c(
    "Interlake-Eastern",
    "Northern",
    "Prairie Mountain Health",
    "Southern Health-Sant\u00E9 Sud", # unicode
    "Winnipeg"
  )

  # process datasets
  switch(
    uuid,
    "0261e07b-85ce-4952-99d1-6e1e9a440291" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::select(
                  .data$RHA,
                  .data$Total_Cases
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
                  value = .data$Total_Cases
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
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::select(
                  .data$RHA,
                  .data$Deaths
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
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
                dplyr::filter(.data$Area == "All") %>%
                dplyr::select(
                  .data$Recovered
                ) %>%
                dplyr::rename(
                  value = .data$Recovered
                ) %>%
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
                dplyr::filter(.data$Area == "All") %>%
                dplyr::select(
                  .data$Total_Tests
                ) %>%
                dplyr::rename(
                  value = .data$Total_Tests
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "1e9f40b2-853f-49d5-a9c4-ed04fee1bea2" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Manufacturer == "All") %>%
                dplyr::select(.data$Doses_Received) %>%
                dplyr::rename(value = .data$Doses_Received) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "a57dc10d-7139-4164-9042-eb2242716585" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(
                  .data$RHA,
                  .data$Total_Doses_Administered
                  ) %>%
                dplyr::filter(.data$RHA != "All") %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
                  value = .data$Total_Doses_Administered
                  ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(
                  .data$RHA,
                  .data$Second_Doses_Administered
                ) %>%
                dplyr::filter(.data$RHA != "All") %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
                  value = .data$Second_Doses_Administered
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "7a5ef226-2244-47e0-b964-28c2dc06d5ae" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(value = .data$Total_Doses_Administered) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "8cb83971-19f0-4dfc-b832-69efc1036ddd" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "hr_cum_current" = {
                ds$features$attributes %>%
                dplyr::select(.data$Area, .data$Total_Hospitalizations) %>%
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::rename(sub_region_1 = .data$Area,
                              value = .data$Total_Hospitalizations) %>%
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
                dplyr::select(.data$Area, .data$Total_ICU_Patients) %>%
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::rename(sub_region_1 = .data$Area,
                              value = .data$Total_ICU_Patients) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
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
