#' maps UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_maps_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      div(id = ns("map_container"), style = "display: none;"),
      column(6, maplibreOutput(ns("map_etablissements_location"))),
      column(6, maplibreOutput(ns("map_by_dept")))
    )
  )
}

#' maps Server Functions
#'
#' @noRd
mod_maps_server <- function(id, rv, dept_sf) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(
      rv$swm_etablissements_with_selected_ccam,
      {
        if (isTruthy(rv$swm_etablissements_with_selected_ccam)) {
          shinyjs::runjs(sprintf(
            '$("%s").show()',
            paste0("#", ns("map_container"))
          ))
        } else {
          shinyjs::runjs(sprintf(
            '$("%s").hide()',
            paste0("#", ns("map_container"))
          ))
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    output$map_etablissements_location <- renderMaplibre({
      req(rv$swm_etablissements_with_selected_ccam)

      maplibre(bounds = dept_sf) %>%
        add_circle_layer(
          id = "swm_etablissements_with_selected_ccam",
          source = rv$swm_etablissements_with_selected_ccam,
          popup = "nom_du_compte",
          circle_color = "red",
          circle_stroke_color = "white",
          circle_stroke_width = 1,
          cluster_options = cluster_options(
            cluster_radius = 30,
            color_stops = c("#377eb8", "#4daf4a", "#984ea3"),
            count_stops = c(0, 200, 500),
            circle_blur = 0.2,
            circle_stroke_color = "white",
            circle_stroke_width = 5
          )
        )
    })

     output$map_by_dept <- renderMaplibre({
      req(rv$swm_etablissements_with_selected_ccam)

      dept_sf_with_count <- left_join(
        dept_sf,
        rv$stats_swm_selected_ccam,
        by = c("code" = "dep")
      ) %>%
        rename(
          n_etablissements_swm = n_etablissements,
          total_nb_actes_swm = total_nb_actes
        ) %>%
        left_join(
          rv$stats_nationales_selected_ccam,
          by = c("code" = "dep")
        ) %>%
        rename(
          n_etablissements_nationales = n_etablissements,
          total_nb_actes_nationales = total_nb_actes
        ) %>%
        mutate(
          n_etablissements_swm_noNA = if_else(
            is.na(n_etablissements_swm),
            0,
            n_etablissements_swm
          )
        ) %>%
        mutate(
          total_nb_actes_swm_noNA = if_else(
            is.na(total_nb_actes_swm),
            0,
            total_nb_actes_swm
          )
        ) %>%
        mutate(
          n_etablissements_nationales_noNA = if_else(
            is.na(n_etablissements_nationales),
            0,
            n_etablissements_nationales
          )
        ) %>%
        mutate(
          total_nb_actes_nationales_noNA = if_else(
            is.na(total_nb_actes_nationales),
            0,
            total_nb_actes_nationales
          )
        ) %>%
        mutate(
          ratio_etablissements_swm_nationales = if_else(
            n_etablissements_nationales_noNA == 0,
            0,
            n_etablissements_swm_noNA / n_etablissements_nationales_noNA
          )
        ) %>%
        mutate(
          ratio_actes_swm_nationales = if_else(
            total_nb_actes_nationales_noNA == 0,
            0,
            total_nb_actes_swm_noNA / total_nb_actes_nationales_noNA
          )
        ) %>%
        mutate(
          ratio_etablissements_swm_nationales_percent = paste0(
            format(
              100 * ratio_etablissements_swm_nationales,
              digits = 2,
              nsmall = 2
            ),
            "%"
          )
        ) %>%
        mutate(
          ratio_actes_swm_nationales_percent = paste0(
            format(100 * ratio_actes_swm_nationales, digits = 2, nsmall = 2),
            "%"
          )
        ) %>%
        mutate(
          popup = paste0(
            "<b>Département: </b>",
            code,            
            "<br><b>Nombre d'actes SWM: </b>",
            total_nb_actes_swm_noNA,
            "<br><b>Nombre d'actes France: </b>",
            total_nb_actes_nationales_noNA,
            "<br><b>Ratio actes SWM/France: </b>",
            ratio_actes_swm_nationales_percent,
            "<br><b>Nombre d'établissements SWM: </b>",
            n_etablissements_swm_noNA,
            "<br><b>Nombre d'établissements France: </b>",
            n_etablissements_nationales_noNA,
            "<br><b>Ratio établissements SWM/France: </b>",
            ratio_etablissements_swm_nationales_percent
          )
        )

      dept_sf_with_count$ratio_actes_swm_nationales_percent <- dept_sf_with_count$ratio_actes_swm_nationales*100
      dept_sf_with_count$ratio_actes_swm_nationales_percent[dept_sf_with_count$ratio_actes_swm_nationales_percent == 0] <- NA_real_

has_any_value <- any(!is.na(dept_sf_with_count$ratio_actes_swm_nationales_percent))
      map <- maplibre(bounds = dept_sf_with_count) %>%
        add_fill_layer(
          id = "dept_sf_with_count",
          source = dept_sf_with_count,
          popup = "popup",
          fill_color = interpolate(
            column = "ratio_actes_swm_nationales_percent",
            values = c(
              min(dept_sf_with_count$ratio_actes_swm_nationales_percent, na.rm = TRUE),
              max(dept_sf_with_count$ratio_actes_swm_nationales_percent, na.rm = TRUE)
            ),
            stops = c("yellow", "darkred"),
            na_color = "grey"
          ),
          fill_opacity = 0.7
        )
        
        if(has_any_value) {
          map <- map %>%
        add_legend(
          legend_title = "Ratio actes SWM/France",
          colors = c("yellow", "darkred"),
          values = c(
            paste0(format(min(dept_sf_with_count$ratio_actes_swm_nationales_percent, na.rm = TRUE), digits = 2, nsmall = 2), "%"),
            paste0(format(max(dept_sf_with_count$ratio_actes_swm_nationales_percent, na.rm = TRUE), digits = 2, nsmall = 2), "%")
          )
        )
        }
        map
    })
  })
}

## To be copied in the UI
# mod_maps_ui("maps_1")

## To be copied in the server
# mod_maps_server("maps_1", rv, dept_sf)
