function(input, output, session) {

  referentiel_actes_csv_path <- file.path(
    here::here("external_data"),
    "referentiel_actes.csv"
  )
  open_ccam_csv_path <- file.path(
    here::here("external_data"),
    "open_ccam/open_ccam_24.csv"
  )

  referentiel_actes_csv <- data.table::fread(referentiel_actes_csv_path)
  open_ccam_csv <- data.table::fread(open_ccam_csv_path)

  swm_sf <- readRDS(file.path(here::here("data"), "swm_cleaned_by_finess_sf.rds"))

  dept_sf <- sf::read_sf(
    here::here("external_data", "departements.geojson")
  ) %>%
    sf::st_transform(4326)

  all_thematics_csv <- list.files(
    here::here("external_data", "specialites"),
    full.names = TRUE
  )

  all_thematics_codes <- lapply(all_thematics_csv, function(x) {
    suppressWarnings(
      csv_thematique <- data.table::fread(x, quote = '"')
    )
    unlist(csv_thematique[, 1])
  })
  names(all_thematics_codes) <- gsub(".csv", "", basename(all_thematics_csv))

  # Other server logic
  rv <- reactiveValues(
    ccam = NULL,
    filtered_referentiel = NULL,
    stats_nationales_selected_ccam = NULL,
    stats_swm_selected_ccam = NULL
  )

  output$selected_ccam <- renderDT({
    validate(
      need(isTruthy(rv$ccam), "Aucun acte CCAM sélectionné")
    )
    DT::datatable(
      rv$filtered_referentiel,
      options = list(pageLength = 10, lengthChange = FALSE)
    )
  })
  mod_ccam_select_server("ccam1", referentiel_actes_csv, rv, all_thematics_codes)

  mod_filter_open_ccam_server("filter_open_ccam_1", rv, open_ccam_csv, swm_sf)
  mod_maps_server("maps_1", rv, dept_sf)
}
