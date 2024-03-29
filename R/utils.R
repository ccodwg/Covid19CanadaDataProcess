#' Helper functions for process_dataset
#'
#' Helper functions for \code{\link{process_dataset}}
#'
#' @name process_dataset_helpers
NULL

#' process_dataset: Common processing for fmt = cum_current
#' @param .data The dataset to be processed.
#' @param loc One of "prov", "hr" or "subhr", depending on the spatial resolution.
#' @param val The value.
#' @param prov The province.
#' @param date_current The date provided to cum_current (usually the current date).
#' @param hr The health region, if providing data for one specific health region only. Used for ON PHU data only.
#' @rdname process_dataset_helpers
#' @export
helper_cum_current <- function(.data, loc = c("prov", "hr", "subhr"),
                               val, prov, date_current, hr = NULL) {
  match.arg(loc, choices = c("prov", "hr", "subhr"), several.ok = FALSE)
  # add common columns
  d <- dplyr::mutate(
    .data,
    province = prov,
    date = date_current,
    value = as.integer(.data$value)
    )
  # replace "name" with "val" if "val" is supplied
  if (!is.null(val)) d <- dplyr::mutate(d, name = val)
  if (loc == "prov") {
    d <- d %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$date,
        .data$value) %>%
      dplyr::arrange(
        .data$date,
        .data$province,
        .data$name)
  } else if (loc == "hr") {
    d <- d %>%
      {if (!is.null(hr)) {
        dplyr::mutate(., sub_region_1 = hr)
    } else {.}} %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$sub_region_1,
        .data$date,
        .data$value) %>%
      dplyr::arrange(
        .data$date,
        .data$province,
        .data$sub_region_1,
        .data$name)
    } else {
      d <- d %>%
        dplyr::mutate(sub_region_1 = hr) %>%
        dplyr::select(
          .data$name,
          .data$province,
          .data$sub_region_1,
          .data$sub_region_2,
          .data$date,
          .data$value) %>%
        dplyr::arrange(
          .data$date,
          .data$province,
          .data$sub_region_1,
          .data$sub_region_2,
          .data$name)
    }
  # convert column names, if applicable
  d <- convert_col_names(d)
  # return dataset
  d
}

#' process_dataset: Common processing for fmt = ts
#' @param .data The dataset to be processed.
#' @param loc One of "prov" or "hr", depending on the spatial resolution.
#' @param val The value.
#' @param prov The province.
#' @param convert_to_cum Convert values to cumulative values? Default: FALSE.
#' @rdname process_dataset_helpers
#' @export
helper_ts <- function(.data, loc = c("prov", "hr"),
                      val, prov, convert_to_cum = FALSE) {
  match.arg(loc, choices = c("prov", "hr"), several.ok = FALSE)
  if (!inherits(.data$date, "Date")) {stop("Make sure the date variable is formatted as Date.")}
  date_seq <- seq.Date(from = min(.data$date), to = max(.data$date), by = "day")
  date_n <- length(date_seq)
  if (loc == "prov") {
    d <- dplyr::mutate(
      .data,
      name = val,
      province = prov,
      value = as.integer(.data$value)
    ) %>%
      dplyr::arrange(.data$name, .data$date) %>%
      {if (convert_to_cum) {
        dplyr::mutate(., value = cumsum(.data$value))
      } else {
        .
      }} %>%
      dplyr::right_join(
        data.frame(
          name = val,
          province = prov,
          date = date_seq
        ),
        by = c("name", "province", "date")
      ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$date,
        .data$value) %>%
      dplyr::arrange(.data$name, .data$date) %>%
      tidyr::fill(.data$value, .direction = "down") %>%
      tidyr::replace_na(list(value = 0)) %>%
      dplyr::mutate(value = as.integer(.data$value))
  } else {
    sub_region_1_names <- sort(unique(.data$sub_region_1))
    sub_region_1_n <- length(sub_region_1_names)
    d <- dplyr::mutate(
      .data,
      name = val,
      province = prov,
      value = as.integer(.data$value)
    ) %>%
      dplyr::arrange(.data$name, .data$date, .data$sub_region_1) %>%
      {if (convert_to_cum) {
        dplyr::group_by(., .data$sub_region_1) %>%
          dplyr::mutate(value = cumsum(.data$value)) %>%
          dplyr::ungroup()
      } else {
        .
      }} %>%
      dplyr::right_join(
        data.frame(
          name = val,
          province = prov,
          sub_region_1 = rep(sub_region_1_names, times = date_n),
          date = rep(date_seq, each = sub_region_1_n)
        ),
        by = c("name", "province", "sub_region_1", "date")
      ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$sub_region_1,
        .data$date,
        .data$value) %>%
      dplyr::arrange(.data$name, .data$date, .data$sub_region_1) %>%
      dplyr::group_by(.data$sub_region_1) %>%
      tidyr::fill(.data$value, .direction = "down") %>%
      tidyr::replace_na(list(value = 0)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = as.integer(.data$value))
  }
  # convert column names, if applicable
  d <- convert_col_names(d)
  # return dataset
  d
}

#' process_dataset: Common processing for fmt = ts for data with multiple provinces (e.g., Canada-wide data)
#' @param d The dataset to be processed.
#' @param val The value.
#' @param convert_to_cum Convert values to cumulative values? Default: FALSE.
#' @param val_numeric Is the value a float (versus an integer)? Default: FALSE.
#' @param keep_update If there is an `update` column, should it be kept? Default: FALSE.
#' @rdname process_dataset_helpers
#' @export
helper_ts_can <- function(d, val, convert_to_cum = FALSE, val_numeric = FALSE, keep_update = FALSE) {
  # check date column type and existence of region column
  if (!inherits(d$date, "Date")) {stop("Make sure the date variable is formatted as Date.")}
  if (!"region" %in% names(d)) {stop("Make sure your province column is named 'region'.")}
  # check existence of updated column
  if (keep_update) {
    if (!"update" %in% names(d)) {
      warning("No column called 'update', ignoring 'keep_update = TRUE'...")
      keep_update <- FALSE
    }
  }
  # data processing
  date_seq <- seq.Date(from = min(d$date), to = max(d$date), by = "day")
  date_n <- length(date_seq)
  provs <- sort(unique(d$region))
  prov_n <- length(provs)
  d <- d %>%
    dplyr::mutate(name = val)
  if (val_numeric) {
    d <- d %>%
      dplyr::mutate(value = as.numeric(.data$value))
  } else {
    d <- d %>%
      dplyr::mutate(value = as.integer(.data$value))
  }
  d <- d %>%
    dplyr::arrange(.data$name, .data$region, .data$date) %>%
    {if (convert_to_cum) {
      dplyr::group_by(., .data$region) %>%
        dplyr::mutate(., value = cumsum(.data$value)) %>%
        dplyr::ungroup()
    } else {
      .
    }} %>%
    dplyr::right_join(
      data.frame(
        name = val,
        region = rep(provs, each = date_n),
        date = rep(date_seq, times = prov_n)
      ),
      by = c("name", "region", "date")
    )
  if (keep_update) {
    d <- d %>%
      dplyr::select(
        .data$name,
        .data$region,
        .data$date,
        .data$value,
        .data$update)
  } else {
    d <- d %>%
      dplyr::select(
        .data$name,
        .data$region,
        .data$date,
        .data$value)
  }
  d <- d %>%
    dplyr::arrange(.data$name, .data$region, .data$date) %>%
    dplyr::group_by(.data$region) %>%
    tidyr::fill(.data$value, .direction = "down") %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::ungroup()
  if (val_numeric) {
    d <- d %>%
      dplyr::mutate(value = as.numeric(.data$value))
  } else {
    d <- d %>%
      dplyr::mutate(value = as.integer(.data$value))
  }
  # return data
  d
}

# process_dataset: province abbreviation to province name (PHAC) or vice versa
#' @param p The column to be processed.
#' @param mode One of "to_phac" or "from_phac". Convert between abbreviated
#' province names (e.g., ON) or the full province names used by PHAC
#' (e.g., Ontario).
#' @rdname process_dataset_helpers
#' @export
phac_prov <- function(p, mode = c("to_phac", "from_phac")) {
  match.arg(mode, choices = c("to_phac", "from_phac"), several.ok = FALSE)
  if (mode == "to_phac") {
    dplyr::case_when(
      # p == "CAN" ~ "Canada", # to use, must strip beginning of phrase
      p == "NL" ~ "Newfoundland and Labrador",
      p == "PE" ~ "Prince Edward Island",
      p == "NS" ~ "Nova Scotia",
      p == "NB" ~ "New Brunswick",
      p == "QC" ~ "Quebec",
      p == "ON" ~ "Ontario",
      p == "MB" ~ "Manitoba",
      p == "SK" ~ "Saskatchewan",
      p == "AB" ~ "Alberta",
      p == "BC" ~ "British Columbia",
      p == "YT" ~ "Yukon",
      p == "NT" ~ "Northwest Territories",
      p == "NU" ~ "Nunavut"#,
      # p == "" ~ "Federal allocation", # to use, must strip anything after (e.g., footnote)
    )
  } else {
    dplyr::case_when(
      p == "Canada" ~ "CAN",
      p == "Newfoundland and Labrador" ~ "NL",
      p == "Prince Edward Island" ~ "PE",
      p == "Nova Scotia" ~ "NS",
      p == "New Brunswick" ~ "NB",
      p == "Quebec" ~ "QC",
      p == "Ontario" ~ "ON",
      p == "Manitoba" ~ "MB",
      p == "Saskatchewan" ~ "SK",
      p == "Alberta" ~ "AB",
      p == "British Columbia" ~ "BC",
      p == "Yukon" ~ "YT",
      p == "Northwest Territories" ~ "NT",
      p == "Nunavut" ~ "NU",
      p == "Repatriated travellers" ~ "RT"
    )
  }
}

#' Error functions for process_dataset
#'
#' Error functions for \code{\link{process_dataset}}
#'
#' @name process_dataset_e
NULL

#' process_dataset: Report no functions to process specified UUID
#' @rdname process_dataset_e
e_uuid <- function() stop("No functions exist to process this UUID.")

#' process_dataset: Report value cannot be extracted from specified UUID
#' @rdname process_dataset_e
e_val <- function() stop("The specified value cannot be extracted from this UUID.")

#' process_dataset: Report value cannot be extracted with specified output format
#' @rdname process_dataset_e
e_fmt <- function() stop("The specified output format is not available for this value.")

#' Convert column names from the old format to the new format
#' @param x A data frame.
#' @return A data frame with renamed columns, if applicable.
#' @noRd
convert_col_names <- function(x) {
  col_names <- c(
    # unchanged names
    name = "name",
    sub_region_1 = "sub_region_1",
    sub_region_2 = "sub_region_2",
    date = "date",
    value = "value",
    value_daily = "value_daily",
    # changed names
    region = "region",
    province = "region"
  )
  names(x) <- col_names[names(x)]
  x
}
