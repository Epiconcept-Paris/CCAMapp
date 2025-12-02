#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DT renderDT
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  library(shiny)
  library(DBI)
  library(duckdb)

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

  rv <- reactiveValues(
    ccam = NULL,
    filtered_table = NULL
  )

  selected <- mod_ccam_select_server("ccam1", con, rv, csv_duckdb)

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
}
