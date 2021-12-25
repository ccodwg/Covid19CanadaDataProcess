#' Functions to process datasets: SK
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_sk <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "SK"

  # process datasets
  switch(
    uuid,
    "95de79d5-5e5c-45c2-bbab-41daf3dbee5d" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 2))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                  ) %>%
              dplyr::mutate(value = as.integer(.data$value))
            # append missing HR data (diff between SK total and sum of HRs)
            dat <- dat %>%
              dplyr::add_row(
                sub_region_1 = "Not Reported",
                value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][2]) -
                  sum(dat$value)
            )
            dat %>%
              helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 7))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(.data$value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][7]) -
                    sum(dat$value)
                )
              dat %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 6))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(.data$value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][6]) -
                    sum(dat$value)
                )
              dat %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "9736bff9-4bd3-4c04-b9d9-87f60b3d5eb5" = {
      match.arg(testing_type, c("n_tests_completed", "n_people_tested"), several.ok = FALSE)
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "hr_cum_current" = {
              if (testing_type == "n_tests_completed") {
                # get HR data
                dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13]
                # get SK total
                sk_total <- as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][2])
              } else if (testing_type == "n_people_tested") {
                # get HR data
                dat <- ds$tabs$tables[[3]]$body[[2]]$cells[1:13]
                # get SK total
                sk_total <- as.integer(ds$tabs$tables[[3]]$body[[2]]$cells[[14]][[1]][2])
              }
              dat <- dat %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 2))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(.data$value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = sk_total -
                    sum(dat$value)
                )
              dat %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "15556169-0471-49ea-926e-20b5e8dbd25d" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "hr_cum_current" = {
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 4))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(.data$value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][4]) -
                    sum(dat$value)
                )
              dat %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "hr_cum_current" = {
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 3))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(.data$value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][3]) -
                    sum(dat$value)
                )
              dat %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "61cfdd06-7749-4ae6-9975-d8b4f10d5651" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::select(.data$Region, .data$Date, .data$Total.Cases) %>%
                dplyr::mutate(Date = as.Date(.data$Date, "%Y/%m/%d")) %>%
                dplyr::group_by(.data$Date, .data$Region) %>%
                dplyr::summarize(Total.Cases = sum(.data$Total.Cases, na.rm = TRUE), .groups = "drop_last") %>%
                dplyr::arrange(.data$Date, .data$Region) %>%
                tidyr::fill(.data$Total.Cases, .direction = "down") %>%
                dplyr::rename(
                  date = .data$Date,
                  sub_region_1 = .data$Region,
                  value = .data$Total.Cases
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
                dplyr::select(.data$Region, .data$Date, .data$Deaths) %>%
                dplyr::mutate(Date = as.Date(.data$Date, "%Y/%m/%d")) %>%
                dplyr::group_by(.data$Date, .data$Region) %>%
                dplyr::summarize(Deaths = sum(.data$Deaths, na.rm = TRUE), .groups = "drop_last") %>%
                dplyr::arrange(.data$Date, .data$Region) %>%
                tidyr::fill(.data$Deaths, .direction = "down") %>%
                dplyr::rename(
                  date = .data$Date,
                  sub_region_1 = .data$Region,
                  value = .data$Deaths
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "28d7f978-9a7b-4933-a520-41b073868d05" = {
      switch(
        val,
        "vaccine_additional_doses" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                {.[[grep("Total Extra Doses", .)[1]]]} %>%
                dplyr::filter(.data$`Age Range` == "Total") %>%
                dplyr::pull(.data$`Total Extra Doses`) %>%
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
        e_val()
      )
    },
    "6e5dd7b2-c6b8-4fd0-8236-ef34873233d2" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "hr_ts" = {
              dat <- do.call(rbind.data.frame,ds[[1]]$tabs$tables[[1]]$body[[2]]$cells)
              dat <- data.frame(matrix(unlist(dat), ncol = 3, byrow = TRUE))

              dat %>% dplyr::filter(!is.na(.data$X1)) %>%
                dplyr::select(.data$X1,.data$X2) %>%
                dplyr::rename( sub_region_1 = .data$X1, value = .data$X2) %>%
                dplyr::mutate(value = as.integer(.data$value)) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "hr_ts" = {
              dat <- do.call(rbind.data.frame,ds[[1]]$tabs$tables[[1]]$body[[2]]$cells)
              dat <- data.frame(matrix(unlist(dat), ncol = 3, byrow = TRUE))

              dat %>% dplyr::filter(!is.na(.data$X1)) %>%
                dplyr::select(.data$X1,.data$X3) %>%
                dplyr::rename( sub_region_1 = .data$X1, value = .data$X3) %>%
                dplyr::mutate(value = as.integer(.data$value)) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
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
