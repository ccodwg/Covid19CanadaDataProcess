#' Helper functions for process_dataset
#'
#' Helper functions for \code{\link{process_dataset}}
#'
#' @name process_dataset_helpers
NULL

#' process_dataset: Common processing for fmt = cum_current
#' @param .data The dataset to be processed.
#' @param loc One of "prov" or "hr", depending on the spatial resolution.
#' @param val The value.
#' @param prov The province.
#' @param date_current The date provided to cum_current (usually the current date).
#' @rdname process_dataset_helpers
helper_cum_current <- function(.data, loc = c("prov", "hr"),
                               val, prov, date_current) {
  match.arg(loc, choices = c("prov", "hr"), several.ok = FALSE)
  if (loc == "prov") {
    dplyr::mutate(
      .data,
      name = val,
      province = prov,
      date = date_current,
      value = as.integer(.data$value)
    ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$date,
        .data$value)
  } else {
    dplyr::mutate(
      .data,
      name = val,
      province = prov,
      date = date_current,
      value = as.integer(.data$value)
    ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$sub_region_1,
        .data$date,
        .data$value)
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

#' Helper functions for webdriver navigation
#'
#' @name webdriver
NULL

#' webdriver_wait_for_element: Wait until element is visible
#' @param webdriver RSelenium server object.
#' @param By Type of selector to use.
#' @param value Value for selector.
#' @param timeout Timeout in seconds.
#' @rdname webdriver
#' @export
webdriver_wait_for_element <- function(webdriver, By, value, timeout) {
  # record start time
  start_time <- Sys.time()

  # check for element
  element <- NULL
  while (is.null(element)) {
    element <- tryCatch({webdriver$client$findElement(using = By, value = value)},
                        error = function(e){NULL})
    # check timeout
    if (as.numeric(Sys.time() - start_time) > timeout) {
      stop("Timeout exceeded while waiting for element to be visible.")
    }
  }

  # return element
  return(element)
}
