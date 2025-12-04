#' filter_open_ccam UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput renderUI
#' @importFrom bslib card card_body
mod_filter_open_ccam_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("nombre_d_etablissements"))
  )
}

#' filter_open_ccam Server Functions
#'
#' @noRd
#' @import mapgl
#' @importFrom dplyr inner_join select distinct
mod_filter_open_ccam_server <- function(id, rv, open_ccam_csv, swm_sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    local_rv <- reactiveValues(
      filtered_open_ccam = NULL
    )
    observeEvent(rv$ccam, {
      req(rv$ccam)
      ccam_codes <- paste0(rv$ccam, "0")

      local_rv$filtered_open_ccam <- open_ccam_csv[acte %in% ccam_codes]

      all(local_rv$filtered_open_ccam$acte %in% ccam_codes)

      rv$stats_nationales_selected_ccam <- local_rv$filtered_open_ccam[,
        .(
          n_etablissements = uniqueN(finessgeo),
          total_nb_actes = sum(nb_actes, na.rm = TRUE)
        ),
        by = dep
      ]

      rv$stats_swm_selected_ccam <- local_rv$filtered_open_ccam[
        finessgeo %in% swm_sf$finess_geographique,
        .(
          n_etablissements = uniqueN(finessgeo),
          total_nb_actes = sum(nb_actes, na.rm = TRUE)
        ),
        by = dep
      ]

      rv$swm_etablissements_with_selected_ccam <- inner_join(
        swm_sf,
        distinct(select(local_rv$filtered_open_ccam, finessgeo)),
        by = c("finess_geographique" = "finessgeo")
      ) %>%
        distinct()
    })

    output$filtered_open_ccam <- DT::renderDT({
      req(local_rv$filtered_open_ccam)
      DT::datatable(
        local_rv$filtered_open_ccam,
        options = list(pageLength = 10, lengthChange = FALSE)
      )
    })

    output$nombre_d_etablissements <- renderUI({
      req(rv$swm_etablissements_with_selected_ccam)
      n_etablissements <- nrow(rv$swm_etablissements_with_selected_ccam)

      div(
        style = "display: flex; justify-content: center; margin: 0 auto;",
        card(
          class = "mb-3",
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none; max-width: 600px; width: 100%;",
          card_body(
            div(
              style = "display: flex; align-items: center; gap: 15px;",
              div(
                style = "font-size: 3rem; font-weight: bold; line-height: 1;",
                n_etablissements
              ),
              div(
                style = "flex: 1;",
                h4(
                  style = "margin: 0; font-weight: 600;",
                  "Établissements trouvés"
                ),
                p(
                  style = "margin: 5px 0 0 0; opacity: 0.9; font-size: 0.9rem;",
                  "avec les codes CCAM sélectionnés"
                )
              )
            )
          )
        )
      )
    })
  })
}
