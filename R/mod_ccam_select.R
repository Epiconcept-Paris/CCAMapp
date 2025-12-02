#' ccam_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput selectizeInput actionButton debounce reactive observeEvent updateSelectizeInput moduleServer reactiveVal
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
      label = "CCAM (max 25 résultats)",
      choices = NULL,
      multiple = TRUE,
      options = list(
        maxOptions = 25,
        closeAfterSelect = FALSE
      )
    ),
    actionButton(
      inputId = ns("select_all"),
      label = "Sélectionner tous les résultats proposés"
    )
  )
}

#' ccam_select Server Functions
#'
#' @noRd
#' @importFrom dplyr collect filter %>% select
#'
mod_ccam_select_server <- function(id, con, rv, csv_duckdb, limit = 25) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Variable réactive pour stocker les choix disponibles
    current_choices <- reactiveVal(NULL)

    search_term <- debounce(reactive(input$search), 200)

    observeEvent(search_term(), {
      req(nchar(search_term()) >= 2)

      q <- paste0("%", search_term(), "%")

      res <- DBI::dbGetQuery(
        con,
        sprintf(
          "
          SELECT COD_ACTE AS code, NOM_COURT AS lib
          FROM ccam
          WHERE COD_ACTE ILIKE ? OR NOM_COURT ILIKE ?
          LIMIT %d
        ",
          limit
        ),
        params = list(q, q)
      )

      choices <- setNames(res$code, paste(res$code, "-", res$lib))
      current_choices(choices)

      updateSelectizeInput(
        session,
        "ccam",
        choices = choices,
        server = TRUE
      )
    })

    observeEvent(input$ccam, {
      rv$ccam <- unique(c(rv$ccam, input$ccam))
      rv$filtered_table <- csv_duckdb %>%
        select(COD_ACTE, NOM_COURT) %>%
        filter(COD_ACTE %in% rv$ccam) %>%
        collect()
    })

    # Gestion du bouton "Select All"
    observeEvent(input$select_all, {
      choices <- current_choices()
      if (!is.null(choices) && length(choices) > 0) {
        updateSelectizeInput(
          session,
          "ccam",
          selected = as.character(choices)
        )
      }
    })
  })
}

## To be copied in the UI
# mod_ccam_select_ui("ccam_select_1")

## To be copied in the server
# mod_ccam_select_server("ccam_select_1")
