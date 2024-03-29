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
                    grepl("^RSS", .data$Croisement) & # gets rid of strange lines "Te" and "Un"
                    .data$Croisement != "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::select(.data$Nom, .data$Date, .data$psi_cum_pos_reinf_n) %>%
                # clean RSS names and convert dates
                dplyr::mutate(
                  Nom = sub("\\d{2} - ", "", .data$Nom),
                  Date = as.Date(.data$Date)) %>%
                dplyr::rename(
                  sub_region_1 = .data$Nom,
                  date = .data$Date,
                  value = .data$psi_cum_pos_reinf_n
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
                    grepl("^RSS", .data$Croisement) & # gets rid of strange lines "Te" and "Un"
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
                # deaths from most recent Sunday to Tuesday are not reported
                # in data posted on Wednesday,
                # so we need to remove last three rows from each RSS
                dplyr::group_by(.data$sub_region_1) %>%
                dplyr::slice(1:(dplyr::n() - 3)) %>%
                dplyr::ungroup() %>%
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
                    grepl("^RSS", .data$Croisement) & # gets rid of strange lines "Te" and "Un"
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
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement == "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = as.integer(.data$psi_cum_adm_n)) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement == "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = as.integer(.data$hos_act_tot_n)) %>%
                dplyr::filter(!is.na(.data$value)) %>%
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
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement == "RSS99" & # all of Quebec
                    .data$Date != "Date inconnue"
                ) %>%
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = as.integer(.data$hos_act_si_n)) %>%
                dplyr::filter(!is.na(.data$value)) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "hosp_admissions" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement == "RSS99" & # all of Quebec
                    # note that Hors QC has 0s for this value anyway
                    .data$Date != "Date inconnue") |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = as.integer(.data$hos_quo_tot_pour_n)) |>
                dplyr::filter(!is.na(.data$value)) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        "icu_admissions" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::filter(
                  .data$Regroupement == "R\u00E9gion" & # unicode
                    .data$Croisement == "RSS99" & # all of Quebec
                    # note that Hors QC has 0s for this value anyway
                    .data$Date != "Date inconnue") |>
                dplyr::transmute(
                  date = as.Date(.data$Date),
                  value = as.integer(.data$hos_quo_si_pour_n)) |>
                dplyr::filter(!is.na(.data$value)) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
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
        "vaccine_total_doses" = {
          ds %>%
            dplyr::slice_head(n = 1) %>%
            dplyr::select(value = 3) %>% # ref by position to avoid unicode name
            helper_cum_current(loc = "prov", val, prov, date_current)
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
                # get region rows
                dplyr::filter(grepl("\\d{2} - ", .data$Categorie) |
                                .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec")) %>%
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
                # get region rows
                dplyr::filter(grepl("\\d{2} - ", .data$Categorie) |
                                .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec")) %>%
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
                # get region rows
                dplyr::filter(grepl("\\d{2} - ", .data$Categorie) |
                                .data$Categorie %in% c("R\u00E9gion inconnue", "Hors Qu\u00E9bec")) %>%
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
    ## N.B. Use the `sep = ";"` argument when loading the ds using `dl_dataset` for code-writing purposes
    "d7f9cb92-a83a-4cd3-b955-762410b2cf5c" = {
      switch(
        val,
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              ## by living situation
              ## https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/situation-coronavirus-quebec/
              ds %>%
                dplyr::mutate(value = rowSums(.[c("CHSLD","RPA","Domicile.et.Inconnu","RI.et.Autres")]),
                              date = as.Date(.data[["Date.de.d\u00E9c\u00E8s"]])) %>%
                dplyr::select(.data$date, .data$value) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "d546a7f6-27ac-4305-bad6-b04435211e79" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::filter(.data$Date!="Date inconnue") %>%
                dplyr::select(.data$Date, .data$Nb_Cas_Cumulatif) %>%
                dplyr::mutate(Date = as.Date(.data$Date)) %>%
                dplyr::rename(date = .data$Date, value = .data$Nb_Cas_Cumulatif) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "active" = {
          switch(
            fmt,
            "prov_ts" = {
              ds %>%
                dplyr::filter(.data$Date!="Date inconnue") %>%
                dplyr::select(.data$Date, .data$Nb_Cas_Actifs) %>%
                dplyr::mutate(Date = as.Date(.data$Date)) %>%
                dplyr::rename(date = .data$Date, value = .data$Nb_Cas_Actifs) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_ts" = {
              # these data are identical to UUID: d7f9cb92-a83a-4cd3-b955-762410b2cf5c
              # Nb_Nvx_Deces_Total == total across groups in UUID above
              ds %>%
                dplyr::filter(.data$Date!="Date inconnue") %>%
                dplyr::select(.data$Date, .data$Nb_Nvx_Deces_Total) %>%
                dplyr::mutate(Date = as.Date(.data$Date)) %>%
                dplyr::rename(date = .data$Date, value = .data$Nb_Nvx_Deces_Total) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = TRUE)
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
        "hospitalizations" = {
          switch(
            fmt,
            "prov_ts" = {
              # change in how hospitalizations were counted starting 2020-04-19 (data are stored in separate columns and merged)
              # https://www.inspq.qc.ca/covid-19/donnees/methodologie?accordeon=hospitalisations1&ancre=hospitalisations-en-cours
              colnames(ds) <- ds[23, ]
              ds[-c(1:23), 1:4] %>%
                dplyr::transmute(
                  date = as.Date(.data$Date, "%d/%m/%Y"),
                  value = as.integer(ifelse(.data$Hospits == "", .data$`Hospits (ancien)`, .data$Hospits))) %>%
                dplyr::filter(!is.na(.data$value)) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "icu" = {
          switch(
            fmt,
            "prov_ts" = {
              colnames(ds) <- ds[23, ]
              ds[-c(1:23), 1:4] %>%
                dplyr::transmute(
                  date = as.Date(.data$Date, "%d/%m/%Y"),
                  value = as.integer(.data$SI)) %>%
                dplyr::filter(!is.na(.data$value)) %>%
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "4e04442d-f372-4357-ba15-3b64f4e03fbe" = {
      switch(
        val,
        "vaccine_administration_total_doses" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSES_Total_cumu) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_1" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSE_Numero1_cumu) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_2" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSE_Numero2_cumu) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_3" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSE_Numero3_cumu) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_4" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSE_Numero4_cumu) |>
                helper_ts(loc = "prov", val, prov, convert_to_cum = FALSE)
            },
            e_fmt()
          )
        },
        "vaccine_administration_dose_5plus" = {
          switch(
            fmt,
            "prov_ts" = {
              ds |>
                dplyr::transmute(
                  date = as.Date(.data$date),
                  value = .data$RSS99_DOSE5etplus_cumu) |>
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
