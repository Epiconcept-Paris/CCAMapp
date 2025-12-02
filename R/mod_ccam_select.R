#' ccam_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ccam_select_ui <- function(id) {
   ns <- NS(id)

  tagList(
    textInput(
      inputId = ns("search"),
      label = "Recherche (code ou libellé)",
      placeholder = "Ex : radio, AAQP…"
    ),

    selectizeInput(
      inputId = ns("ccam"),
      label = "CCAM",
      choices = NULL,
      multiple = TRUE,            # ← multi-sélection
      options = list(
        maxOptions = 50,
        closeAfterSelect = FALSE
      )
    )
  )
}
    
#' ccam_select Server Functions
#'
#' @noRd 
mod_ccam_select_server <- function(id, con, rv, limit = 100){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

     search_term <- debounce(reactive(input$search), 200)

    observeEvent(search_term(), {
      req(nchar(search_term()) >= 2)

      q <- paste0("%", search_term(), "%")

      res <- DBI::dbGetQuery(
        con,
        sprintf("
          SELECT COD_ACTE AS code
          FROM ccam
          WHERE COD_ACTE ILIKE ?
          LIMIT %d
        ", limit),
        params = list(q)
      )

      choices <- setNames(res$code, paste(res$code))

      updateSelectizeInput(
        session,
        "ccam",
        choices = choices,
        server = TRUE
      )
    })

    observeEvent(input$ccam, {
      rv$ccam <- unique(c(rv$ccam, input$ccam))
    })
 
  })
}
    
## To be copied in the UI
# mod_ccam_select_ui("ccam_select_1")
    
## To be copied in the server
# mod_ccam_select_server("ccam_select_1")
