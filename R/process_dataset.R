#' Process a dataset catalogued in Covid19CanadaArchive
#'
#' Process a dataset listed in datasets.json of Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/datasets.json)
#' into a standardized format for a specific value provided by that dataset
#' (e.g., cases, deaths, hospitalizations, etc.). The dataset must be provided
#' via `ds` in the format returned by `dl_dataset` from `Covid19CanadaData`.
#'
#' The currently supported values are the following:
#' \itemize{
#'  \item cases
#'  \item mortality
#'  \item recovered
#'  \item active
#'  \item testing
#'  \item vaccine_distribution
#'  \item vaccine_coverage_dose_1
#'  \item vaccine_coverage_dose_2
#'  \item vaccine_coverage_dose_3
#'  \item vaccine_coverage_dose_4
#'  \item vaccine_coverage_dose_5
#'  \item vaccine_coverage_last_6_months
#'  \item vaccine_administration_total_doses
#'  \item vaccine_administration_dose_1
#'  \item vaccine_administration_dose_2
#'  \item vaccine_administration_dose_3
#'  \item vaccine_administration_dose_4
#'  \item vaccine_administration_dose_5plus
#'  \item vaccine_administration_last_6_months
#'  \item hospitalizations
#'  \item hospitalizations_cum
#'  \item hosp_admissions
#'  \item icu
#'  \item icu_admissions
#'  \item wastewater_copies_per_ml
#' }
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param val The desired value to be extracted from the dataset. Valid
#' inputs, include "cases", "mortality", "testing", etc. See details.
#' @param fmt The format of the output data.
#' @param ds The downloaded dataset in the format returned by `dl_dataset`.
#' @param ... Additional arguments to be passed to the processing function.
#' @return A standardized version of the dataset for the desired value.
#'
#' @export
process_dataset <- function(uuid,
                            val = c(
                              "cases",
                              "mortality",
                              "recovered",
                              "active",
                              "testing",
                              "vaccine_coverage_dose_1",
                              "vaccine_coverage_dose_2",
                              "vaccine_coverage_dose_3",
                              "vaccine_coverage_dose_4",
                              "vaccine_coverage_dose_5",
                              "vaccine_coverage_last_6_months",
                              "vaccine_administration_total_doses",
                              "vaccine_administration_dose_1",
                              "vaccine_administration_dose_2",
                              "vaccine_administration_dose_3",
                              "vaccine_administration_dose_4",
                              "vaccine_administration_dose_5plus",
                              "vaccine_administration_last_6_months",
                              "vaccine_distribution_total_doses",
                              "hospitalizations",
                              "hospitalizations_cum",
                              "hosp_admissions",
                              "icu",
                              "icu_admissions",
                              "wastewater_copies_per_ml"
                            ),
                            fmt,
                            ds,
                            ...) {

  # get list of datasets (datasets.json)
  ds_list <- Covid19CanadaData::get_datasets()

  # get list of uuids
  uuids <- names(ds_list)

  # check UUID
  if (uuid %in% uuids) {
    d <- ds_list[[uuid]]
  } else {
    stop("Specified UUID does not exist in datasets.json.")
  }

  # warn about deprecated values:
  if (val %in% c(
    "vaccine_administration",
    "vaccine_completion",
    "vaccine_total_doses",
    "vaccine_dose_1",
    "vaccine_dose_2",
    "vaccine_dose_3",
    "vaccine_dose_4",
    "vaccine_additional_doses",
    "vaccine_distribution"
  )) {
    warning("Value has been deprecated: ", val)
  }

  # for backwards compatibility: rename val
  val <- dplyr::case_when(
    val == "vaccine_administration" ~ "vaccine_total_doses",
    val == "vaccine_completion" ~ "vaccine_dose_2",
    val == "vaccine_distribution" ~ "vaccine_distribution_total_doses",
    TRUE ~ val
  )

  # check value
  match.arg(
    val,
    choices = c("cases",
                "mortality",
                "recovered",
                "active",
                "testing",
                "vaccine_distribution",
                "vaccine_coverage_dose_1",
                "vaccine_coverage_dose_2",
                "vaccine_coverage_dose_3",
                "vaccine_coverage_dose_4",
                "vaccine_coverage_dose_5",
                "vaccine_coverage_last_6_months",
                "vaccine_administration_total_doses",
                "vaccine_administration_dose_1",
                "vaccine_administration_dose_2",
                "vaccine_administration_dose_3",
                "vaccine_administration_dose_4",
                "vaccine_administration_dose_5",
                "vaccine_administration_last_6_months",
                "vaccine_total_doses",
                "vaccine_dose_1",
                "vaccine_dose_2",
                "vaccine_dose_3",
                "vaccine_dose_4",
                "vaccine_additional_doses",
                "hospitalizations",
                "hospitalizations_cum",
                "hosp_admissions",
                "icu",
                "icu_admissions",
                "wastewater_copies_per_ml"),
    several.ok = FALSE
  )

  # get processing function name based on province name identified in datasets.json
  prov <- gsub("/", "-", d$dir_parent)
  process_fun <- get(paste0("process_", prov))

  # unpack optional arguments
  dots <- list(...)
  if (methods::hasArg("prov")) {
    prov <- dots[["prov"]]
  } else {
    prov <- NULL
  }
  if (methods::hasArg("date_current")) {
    date_current <- dots[["date_current"]]
  } else {
    date_current <- lubridate::date(lubridate::with_tz(Sys.time(), "America/Toronto"))
  }
  if (methods::hasArg("testing_type")) {
    testing_type <- dots[["testing_type"]]
  } else {
    testing_type <- NULL
  }
  if (methods::hasArg("hr")) {
    hr <- dots[["hr"]]
  } else {
    hr <- NULL
  }

  # pass arguments to processing function
  dat_processed <- tryCatch(
    {
      # check dataset then run processing function
      if (identical(ds, NA) | identical(ds, NULL)) {
        stop("Expected dataset, got NA or NULL.")
      } else {
        process_fun(uuid, val, fmt, ds, prov, hr, date_current, testing_type)
      }
    },
    error = function(e) {
      uuid_id_name <- tryCatch(Covid19CanadaData::get_uuid(uuid)$id_name, error = function(e) return("Name unknown"))
      warn <- paste(uuid_id_name, uuid, val, fmt, sep = " / ")
      warn <- paste0(e, warn, "\nFailed, returning NA\n")
      message(warn)
      return(NA)
    }
  )

  # return processed data
  dat_processed

}
