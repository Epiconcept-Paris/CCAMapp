#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import duckplyr
#' @importFrom duckdb duckdb
#' @import DBI
#' @import sf
#' @importFrom DT renderDT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Chargement de DuckDB + CSV (sans mise en RAM)
  con <- dbConnect(duckdb::duckdb())

  dbExecute(
    con,
    "
  CREATE TABLE ccam AS
  SELECT * FROM read_csv_auto('inst/referentiel_actes.csv');
"
  )

  csv_duckdb <- duckplyr::read_csv_duckdb(
    "inst/referentiel_actes.csv",
    options = list(ignore_errors = FALSE)
  )

  csv_open_ccam <- duckplyr::read_csv_duckdb(
    "inst/open_ccam/open_ccam_24.csv",
    options = list(ignore_errors = FALSE)
  )

  dept_sf <- sf::read_sf(
    "inst/departements.geojson"
  ) %>%
    sf::st_transform(4326)

  rv <- reactiveValues(
    ccam = NULL,
    filtered_table = NULL
  )

  mod_ccam_select_server("ccam1", con, rv, csv_duckdb)

  output$out <- renderDT({
    req(rv$filtered_table)
    DT::datatable(
      rv$filtered_table,
      options = list(pageLength = 10, lengthChange = FALSE)
    )
  })

  observeEvent(input$erase_selection, {
    rv$ccam <- NULL
    rv$filtered_table <- NULL
  })

  mod_filter_open_ccam_server("filter_open_ccam_1", rv, csv_open_ccam, dept_sf)
  mod_maps_server("maps_1", rv, dept_sf)
}
