#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
library(shiny)
library(DBI)
library(duckdb)

# Chargement de DuckDB + CSV (sans mise en RAM)
con <- dbConnect(duckdb::duckdb())

dbExecute(con, "
  CREATE TABLE ccam AS
  SELECT * FROM read_csv_auto('inst/referentiel_actes.csv');
")

rv <- reactiveValues(
  ccam = NULL
)

  selected <- mod_ccam_select_server("ccam1", con, rv)

  output$out <- renderPrint({
    rv$ccam
  })
}
