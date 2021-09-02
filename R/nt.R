#' Functions to process datasets: NT
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nt <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {

  # set defaults
  prov <- "NT"

  # process datasets
  switch(
    uuid,
    "e008ba6f-7b09-4af4-afc5-63ec3c3bbfbb" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements(".metric") %>%
                {.[grep("Confirmed Cases", .)]} %>%
                rvest::html_text() %>%
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
              data.frame(
                value = 0 # deaths are not reported
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
                rvest::html_elements(".metric") %>%
                {.[grep("Resolved Cases", .)]} %>%
                rvest::html_text() %>%
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
    "66fbe91e-34c0-4f7f-aa94-cf6c14db0158" = {
      url <- Covid19CanadaData::get_dataset_url("66fbe91e-34c0-4f7f-aa94-cf6c14db0158")
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              web <- Covid19CanadaData::webdriver_open(url)
              # click on testing tab (when element is visible)
              elm <- webdriver_wait_for_element(
                web,
                "xpath",
                "/html/body/div[1]/nav/div/ul/li[2]/a",
                10)
              # wait for page to load before clicking tab
              Sys.sleep(5)
              elm$clickElement()
              # extract HTML
              Sys.sleep(10) # wait for page to load
              ds <- web$client$getPageSource()
              # tidy up
              Covid19CanadaData::webdriver_close(web)
              # parse HTML
              ds[[1]] %>%
                rvest::read_html() %>%
                rvest::html_element("#complete") %>%
                rvest::html_text2() %>%
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
    "454de458-f7b4-4814-96a6-5a426f8c8c60" = {
      url <- Covid19CanadaData::get_dataset_url("454de458-f7b4-4814-96a6-5a426f8c8c60")
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              web <- Covid19CanadaData::webdriver_open(url)
              # click on vaccine doses tab dropdown (when element is visible)
              elm <- webdriver_wait_for_element(
                web,
                "xpath",
                "/html/body/div[1]/nav/div/ul/li[4]/a",
                10)
              # wait for page to load before clicking tab
              Sys.sleep(5)
              elm$clickElement()
              # click tab
              webdriver_wait_for_element(
                web,
                "xpath",
                "/html/body/div[1]/nav/div/ul/li[4]/ul/li[2]/a",
                10)$clickElement()
              # extract HTML
              Sys.sleep(10) # wait for page to load
              ds <- web$client$getPageSource()
              # tidy up
              Covid19CanadaData::webdriver_close(web)
              # parse HTML
              ds[[1]] %>%
                rvest::read_html() %>%
                rvest::html_element("#totaldoses") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
                readr::parse_number() %>%
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
              web <- Covid19CanadaData::webdriver_open(url)
              # click on vaccine doses tab dropdown (when element is visible)
              elm <- webdriver_wait_for_element(
                web,
                "xpath",
                "/html/body/div[1]/nav/div/ul/li[4]/a",
                10)
              # wait for page to load before clicking tab
              Sys.sleep(5)
              elm$clickElement()
              # click tab
              webdriver_wait_for_element(
                web,
                "xpath",
                "/html/body/div[1]/nav/div/ul/li[4]/ul/li[2]/a",
                10)$clickElement()
              # extract HTML
              Sys.sleep(10) # wait for page to load
              ds <- web$client$getPageSource()
              # tidy up
              Covid19CanadaData::webdriver_close(web)
              # parse HTML
              ds[[1]] %>%
                rvest::read_html() %>%
                rvest::html_element("#totalseconddose") %>%
                rvest::html_text2() %>%
                # get everything after last \n
                stringr::str_extract("[^\n]*$") %>%
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
