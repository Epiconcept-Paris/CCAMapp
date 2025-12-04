library(shiny)
library(data.table)
library(dplyr)
library(DT)
library(mapgl)
library(sf)
library(shinyjs)
library(bslib)
library(tidyr)
library(ggplot2)

ui <- page_sidebar(
  useShinyjs(),
  title = "Exploration actes CCAM",
  sidebar = sidebar(
    mod_ccam_select_ui("ccam1")
  ),
  tagList(
    fluidRow(
      style = "display: flex; align-items: stretch;",
      column(4,
        style = "display: flex; flex-direction: column;",
        h2("Actes CCAM sélectionnés"),
        div(
          style = "flex: 1; overflow: auto; min-height: 400px;",
          DTOutput("selected_ccam")
        )
      ),
      column(8,
        style = "display: flex; flex-direction: column;",
        h2("Actes CCAM sélectionnés avec thématiques"),
        div(
          style = "flex: 1; min-height: 400px;",
          plotOutput("selected_ccam_with_categories", height = "100%")
        )
      )
    ),
    fluidRow(mod_filter_open_ccam_ui("filter_open_ccam_1")),
    fluidRow(mod_maps_ui("maps_1"))
  )
)

server <- function(input, output, session) {

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

  output$selected_ccam_with_categories <- renderPlot({
      validate(
      need(isTruthy(rv$ccam), "Aucun acte CCAM sélectionné")
    )

    ccam_with_thematics <- lapply(seq_len(length(all_thematics_codes)), function(x) {      
      if (any(rv$ccam %in% all_thematics_codes[[x]])) {
        ccam_present_in_thematique <- intersect(rv$ccam, all_thematics_codes[[x]])
        list(ccam_present_in_thematique = ccam_present_in_thematique, thematique = names(all_thematics_codes)[x])
      } else {
        return(NULL)
      }
    })
  ccam_with_thematics <- data.table::rbindlist(ccam_with_thematics)
  selected_ccam_with_thematics <- data.table::data.table(code = rv$ccam)
  selected_ccam_with_thematics <- merge(selected_ccam_with_thematics, ccam_with_thematics, by.x = "code", by.y = "ccam_present_in_thematique", all.x = TRUE)
  selected_ccam_with_thematics <- selected_ccam_with_thematics[is.na(thematique), thematique := "Thématique inconnue"]
  thematique_levels <- unique(selected_ccam_with_thematics$thematique)
  thematique_levels <- c("Thématique inconnue", setdiff(thematique_levels, "Thématique inconnue"))
  selected_ccam_with_thematics$thematique <- factor(selected_ccam_with_thematics$thematique, levels = thematique_levels)
  selected_ccam_with_thematics <- selected_ccam_with_thematics[order(thematique)]

  # Compter les actes par thématique
  thematique_counts <- selected_ccam_with_thematics[, .N, by = thematique]
  thematique_counts <- thematique_counts[order(-N)]
  
  ggplot(thematique_counts, aes(x = reorder(thematique, N), y = N)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
    geom_text(aes(label = N), hjust = -0.2, size = 5, color = "black") +
    coord_flip() +
    theme_minimal() +
    xlab("") +
    ylab("Nombre d'actes CCAM") +
    theme(
      axis.text.y = element_text(size = 13, hjust = 1), 
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 16, margin = margin(t = 10)),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey90", linetype = "dashed"),
      plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt"),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  })
  mod_ccam_select_server("ccam1", referentiel_actes_csv, rv, all_thematics_codes)

  mod_filter_open_ccam_server("filter_open_ccam_1", rv, open_ccam_csv, swm_sf, dept_sf)
  mod_maps_server("maps_1", rv, dept_sf)
}

shinyApp(ui = ui, server = server)