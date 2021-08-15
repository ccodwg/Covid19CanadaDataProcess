#' Functions to process datasets: ON
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_on <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {

  # set defaults
  prov <- "ON"

  # process datasets
  switch(
    uuid,
    "" = {
      switch(
        val,
        "" = {
          switch(
            fmt,
            "" = {

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
