#' Special data processing functions
#'
#' Some datasets are such that they cannot be processed via the usual pipeline
#' (using \code{\link[Covid19CanadaDataProcess]{process_dataset}}).

#' @name process_special
NULL

#' Download and process case and death data from PHO's Ontario COVID-19 Data Tool.
#'
#' @rdname process_special
#'
#' @export
process_pho_data_tool <- function() {
  # download "trends" datasets
  trends1 <- tempfile()
  trends2 <- tempfile()
  trends3 <- tempfile()
  Covid19CanadaData::dl_dataset("75f397c6-ebb2-478e-b8b3-0ee7d4cf2a5d", file = trends1)
  Covid19CanadaData::dl_dataset("6803d544-1aa1-47fa-a045-0e2ead2b995e", file = trends2)
  Covid19CanadaData::dl_dataset("95107720-6285-411d-adad-c154b1d4bf33", file = trends3)
  # get dates
  dates <- Covid19CanadaData::read_dataset(
    file = trends1,
    d = Covid19CanadaData::get_uuid("75f397c6-ebb2-478e-b8b3-0ee7d4cf2a5d"),
    sheet = "dayID") |>
    dplyr::mutate(date = as.Date(.data$date, format = "%d-%m-%Y"))
  # get datasets
  get_pho_data <- function(file, uuid) {
    # get health regions
    hruids <- Covid19CanadaData::get_sheet_names(file)
    hruids <- hruids[grepl("trends\\d{4}", hruids)]
    # function: extract data
    dat <- lapply(hruids, function(x) {
      Covid19CanadaData::read_dataset(
        file,
        d = Covid19CanadaData::get_uuid(uuid),
        sheet = x) |>
        dplyr::transmute(
          sub_region_1 = sub("trends", "", x),
          .data$dayID,
          cases = .data$reportedCases,
          .data$deaths
        )
    }) |>
      dplyr::bind_rows()
  }
  # extract data for all files
  dat <- dplyr::bind_rows(
    get_pho_data(trends1, "75f397c6-ebb2-478e-b8b3-0ee7d4cf2a5d"),
    get_pho_data(trends2, "6803d544-1aa1-47fa-a045-0e2ead2b995e"),
    get_pho_data(trends3, "95107720-6285-411d-adad-c154b1d4bf33")
  )
  # add dates
  dat <- dat |>
    dplyr::left_join(
      dates,
      by = "dayID"
    )
  # case data
  cases_on <- dat |>
    dplyr::transmute(
      .data$sub_region_1,
      .data$date,
      value = .data$cases
    ) |>
    helper_ts(loc = "hr", "cases", "ON", convert_to_cum = TRUE)
  deaths_on <- dat |>
    dplyr::transmute(
      .data$sub_region_1,
      .data$date,
      value = .data$deaths
    ) |>
    helper_ts(loc = "hr", "deaths", "ON", convert_to_cum = TRUE)
  # return datasets as a list
  list(
    cases_on = cases_on,
    deaths_on = deaths_on
  )
  }
