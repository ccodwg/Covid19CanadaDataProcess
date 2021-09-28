#' Functions to process datasets: QC
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_qc <- function(uuid, val, fmt, ds,
                       prov, hr, date_current, testing_type) {

  # set defaults
  prov <- "QC"

  # process datasets
  switch(
    uuid,
    "3b93b663-4b3f-43b4-a23d-cbf6d149d2c5" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement != "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::select(.data$Nom, .data$Date, .data$cas_cum_tot_n) %>%
                # clean RSS names and convert dates
                dplyr::mutate(
                  Nom = sub("\\d{2} - ", "", .data$Nom),
                  Date = as.Date(.data$Date)) %>%
                dplyr::rename(
                  sub_region_1 = .data$Nom,
                  date = .data$Date,
                  value = .data$cas_cum_tot_n
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
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement != "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::select(.data$Nom, .data$Date, .data$dec_cum_tot_n) %>%
                # clean RSS names and convert dates
                dplyr::mutate(
                  Nom = sub("\\d{2} - ", "", .data$Nom),
                  Date = as.Date(.data$Date)) %>%
                dplyr::rename(
                  sub_region_1 = .data$Nom,
                  date = .data$Date,
                  value = .data$dec_cum_tot_n
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_ts" = {
              ds %>%
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement != "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::select(.data$Nom, .data$Date,
                              .data$cas_cum_tot_n, .data$act_cum_tot_n, .data$dec_cum_tot_n) %>%
                # clean RSS names and convert dates
                dplyr::mutate(
                  Nom = sub("\\d{2} - ", "", .data$Nom),
                  Date = as.Date(.data$Date)) %>%
                # calculate recovered
                dplyr::mutate(
                  value =
                    .data$cas_cum_tot_n -
                    .data$act_cum_tot_n -
                    .data$dec_cum_tot_n
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$Nom,
                  date = .data$Date,
                ) %>%
                helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "testing" = {
          match.arg(testing_type, c("n_people_tested", "n_eligible_tests_completed"), several.ok = FALSE)
          switch(
            fmt,
            "hr_ts" = {
              if (testing_type == "n_people_tested") {
                ds %>%
                  dplyr::filter(
                    .data$Regroupement == "R\u00E9gion" & # unicode
                      .data$Croisement != "RSS99" & # all of Quebec
                      .data$Date != "Date inconnue"
                  ) %>%
                  dplyr::select(.data$Nom, .data$Date, .data$psi_cum_tes_n) %>%
                  # clean RSS names and convert dates
                  dplyr::mutate(
                    Nom = sub("\\d{2} - ", "", .data$Nom),
                    Date = as.Date(.data$Date)) %>%
                  dplyr::rename(
                    sub_region_1 = .data$Nom,
                    date = .data$Date,
                    value = .data$psi_cum_tes_n
                  ) %>%
                  helper_ts(loc = "hr", val, prov, convert_to_cum = FALSE)
              } else if (testing_type == "n_eligible_tests_completed") {
                ds %>%
                  dplyr::filter(
                    .data$Regroupement == "R\u00E9gion" & # unicode
                      .data$Croisement != "RSS99" & # all of Quebec
                      .data$Date != "Date inconnue"
                  ) %>%
                  dplyr::select(.data$Nom, .data$Date, .data$psi_quo_tes_n) %>%
                  # clean RSS names and convert dates
                  dplyr::mutate(
                    Nom = sub("\\d{2} - ", "", .data$Nom),
                    Date = as.Date(.data$Date)) %>%
                  dplyr::rename(
                    sub_region_1 = .data$Nom,
                    date = .data$Date,
                    value = .data$psi_quo_tes_n
                  ) %>%
                  helper_ts(loc = "hr", val, prov, convert_to_cum = TRUE)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "b78d46c8-9a56-4b75-94c5-4ace36e014f5" = {
      switch(
        val,
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds[2, 5, drop = FALSE] %>% # select by position
                dplyr::mutate(
                  value = readr::parse_number(
                    gsub(" ", "", .)
                  )) %>%
                dplyr::select(.data$value) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              match.arg(testing_type, c("n_tests_completed"), several.ok = FALSE)
              if (testing_type == "n_tests_completed") {
                ds[24:nrow(ds), 5] %>%
                  as.integer() %>%
                  sum(na.rm = TRUE) %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", val, prov, date_current)
              }
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "aee3bd38-b782-4880-9033-db76f84cef5b" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_head(n = 1) %>%
                dplyr::select(value = 2) %>% # ref by position to avoid unicode name
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_administration" = {
          ds %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::select(value = 3) %>% # ref by position to avoid unicode name
            helper_cum_current(loc = "prov", val, prov, date_current)
        },
        e_val()
      )
    },
    "4e04442d-f372-4357-ba15-3b64f4e03fbe" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_tail(n = 1) %>%
                # dplyr::transmute(value = .data$RSS99_DOSES_Total_cumu) %>%
                dplyr::transmute(value = .data$RSSAL_DOSES_Total_cumu) %>% # includes non-residents
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
                dplyr::slice_tail(n = 1) %>%
                # dplyr::transmute(value = .data$RSS99_DOSE_Numero2_cumu) %>%
                dplyr::transmute(value = .data$RSSAL_DOSE_Numero2_cumu) %>% # includes non-residents
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "939189e0-b7bb-4e8c-b71a-b0b9311b7233" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                # remove provincial total
                dplyr::filter(
                  .data[["R\u00E9gions.sociosanitaires"]] != "Total"
                ) %>%
                # clean RSS names
                dplyr::mutate(
                  "R\u00E9gions.sociosanitaires" = sub(
                    "\\d{2} - ", "", .data[["R\u00E9gions.sociosanitaires"]]),

                  "Total.des.cas.confirm\u00E9s.depuis.le.d\u00E9but.de.la.pand\u00E9mie" = readr::parse_number(
                    gsub("\u00a0", "", .data[["Total.des.cas.confirm\u00E9s.depuis.le.d\u00E9but.de.la.pand\u00E9mie"]])) # note the unicode thousands separator
                    ) %>%
                dplyr::rename(
                  sub_region_1 = .data[["R\u00E9gions.sociosanitaires"]], # unicode
                  value = .data[["Total.des.cas.confirm\u00E9s.depuis.le.d\u00E9but.de.la.pand\u00E9mie"]]
                  ) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$value
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "50d7d98b-d7d1-4e0c-a47d-5983565d17c7" = {
      switch(
        val,
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                # remove provincial total
                dplyr::filter(
                  .data[["R\u00E9gions"]] != "Total"
                ) %>%
                # clean RSS names
                dplyr::mutate(
                  "R\u00E9gions" = sub(
                    "\\d{2} - ", "", .data[["R\u00E9gions"]]),

                  "Nombre.de.d\u00E9c\u00E8s" = readr::parse_number(
                    gsub("\u00a0", "", .data[["Nombre.de.d\u00E9c\u00E8s"]])) # note the unicode thousands separator
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data[["R\u00E9gions"]], # unicode
                  value = .data[["Nombre.de.d\u00E9c\u00E8s"]]
                ) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$value
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "0c577d5e-999e-42c5-b4c1-66b3787c3a04" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::slice(1:20) %>%
                # strip numbers from region names
                dplyr::mutate(
                  Categorie = ifelse(
                    .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec"),
                    .data$Categorie,
                    stringr::str_sub(.data$Categorie, 6, nchar(.data$Categorie)))
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$Categorie,
                  value = .data$Nb_Cas_Cumulatif) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$value
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
              ds %>%
                dplyr::slice(1:20) %>%
                # strip numbers from region names
                dplyr::mutate(
                  Categorie = ifelse(
                    .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec"),
                    .data$Categorie,
                    stringr::str_sub(.data$Categorie, 6, nchar(.data$Categorie)))
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$Categorie,
                  value = .data$Nb_Deces_Cumulatif_Total) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$value
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
              ds %>%
                dplyr::slice(1:20) %>%
                # strip numbers from region names
                dplyr::mutate(
                  Categorie = ifelse(
                    .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec"),
                    .data$Categorie,
                    stringr::str_sub(.data$Categorie, 6, nchar(.data$Categorie)))
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$Categorie,
                  value = .data$Nb_Retablis_Cumulatif) %>%
                dplyr::select(
                  .data$sub_region_1,
                  .data$value
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f0c25e20-2a6c-4f9a-adc3-61b28ab97245" = {
      switch(
        val,
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$Date, .data$ACT_Total_RSS99) %>%
                dplyr::mutate(Date = as.Date(.data$Date)) %>%
                dplyr::rename(date = .data$Date, value = .data$ACT_Total_RSS99) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::select(.data$Date, .data$ACT_Si_RSS99) %>%
                dplyr::mutate(Date = as.Date(.data$Date)) %>%
                dplyr::rename(date = .data$Date, value = .data$ACT_Si_RSS99) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
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
