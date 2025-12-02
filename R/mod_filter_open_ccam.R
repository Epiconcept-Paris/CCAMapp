#' filter_open_ccam UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_filter_open_ccam_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Table Open CCAM filtrée par les CCAM sélectionnés"),
    DTOutput(ns("filtered_open_ccam"))
    
  )
}
    
#' filter_open_ccam Server Functions
#'
#' @noRd 
mod_filter_open_ccam_server <- function(id, rv, csv_open_ccam){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    local_rv <- reactiveValues(
      filtered_open_ccam = NULL
    )
    observeEvent(rv$ccam, {
      local_rv$filtered_open_ccam <- csv_open_ccam %>%
        filter(acte %in% paste0(rv$ccam , "0")) %>%
        collect()
    })

    output$filtered_open_ccam <- DT::renderDT({
      req(local_rv$filtered_open_ccam)
      DT::datatable(
        local_rv$filtered_open_ccam,
        options = list(pageLength = 10, lengthChange = FALSE)
      )
    })
  })
}
