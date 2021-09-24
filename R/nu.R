#' Functions to process datasets: NU
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nu <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {

  # set defaults
  prov <- "NU"

  # process datasets
  switch(
    uuid,
    "04ab3773-f535-42ad-8ee4-4d584ec23523" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$`Total confirmed cases`) %>%
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
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$Deaths) %>%
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
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$`Total recovered cases`) %>%
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
        "testing" = {
          match.arg(testing_type, c("n_tests_completed", "n_people_tested"), several.ok = FALSE)
          switch(
            fmt,
            "prov_cum_current" = {
              if (testing_type == "n_tests_completed") {
                ds %>%
                  rvest::html_table(header = TRUE) %>%
                  `[[`(1) %>%
                  dplyr::select(.data$`Total tests`) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_table(header = TRUE) %>%
                  `[[`(3) %>%
                  dplyr::filter(.data$Community == "Total") %>%
                  dplyr::select(
                    .data$`Tests Positive`,
                    .data$`Tests Negative`
                    ) %>%
                  dplyr::mutate(
                    `Tests Positive` = readr::parse_number(
                      as.character(.data$`Tests Positive`)), # in case someone adds a special character, e.g. "*"
                    `Tests Negative` = readr::parse_number(
                      as.character(.data$`Tests Negative`)),
                    value = .data$`Tests Positive` + .data$`Tests Negative`
                  ) %>%
                  dplyr::select(.data$value) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "bd18a4e4-bc22-47c6-b601-1aae39667a03" = {
      # geometry to extract dose numbers from table image
      dose_1_geom <- c("38x14+679+110")
      dose_2_geom <- c("38x14+790+110")
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              dose_1 <- ds %>%
                # crop to dose 1
                magick::image_crop(geometry = dose_1_geom) %>%
                # increase contrast
                magick::image_transparent("white", fuzz = 10) %>%
                magick::image_background("white") %>%
                # ocr text
                tesseract::ocr() %>%
                # read number
                readr::parse_number()
              dose_2 <- ds %>%
                # crop to dose 2
                magick::image_crop(geometry = dose_2_geom) %>%
                # increase contrast
                magick::image_transparent("white", fuzz = 10) %>%
                magick::image_background("white") %>%
                # ocr text
                tesseract::ocr() %>%
                # read number
                readr::parse_number()
                (dose_1 + dose_2) %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                # crop to dose 2
                magick::image_crop(geometry = dose_2_geom) %>%
                # increase contrast
                magick::image_transparent("white", fuzz = 10) %>%
                magick::image_background("white") %>%
                # ocr text
                tesseract::ocr() %>%
                # read number
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
    e_uuid()
  )
}
