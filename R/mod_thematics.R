mod_thematics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      style = "display: flex; align-items: stretch;",
      column(
        4,
        style = "display: flex; flex-direction: column;",
        h2("Actes CCAM sélectionnés"),
        div(
          style = "flex: 1; overflow: auto; min-height: 400px;",
          DTOutput(ns("selected_ccam"))
        )
      ),
      column(
        8,
        style = "display: flex; flex-direction: column;",
        h2("Actes CCAM sélectionnés classifiés par thématiques"),
        div(
          style = "flex: 1; min-height: 400px;",
          plotOutput(ns("selected_ccam_with_categories"), height = "100%")
        )
      )
    )
  )
}

mod_thematics_server <- function(id, rv, all_thematics_codes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(
      selected_ccam_with_thematics = NULL
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

    observeEvent(rv$ccam, {
        
        selected_ccam_with_thematics <- retrieve_ccam_thematics(rv$ccam, all_thematics_codes)

      local_rv$selected_ccam_with_thematics <- selected_ccam_with_thematics[order(
        thematique
      )]
    })

    output$selected_ccam_with_categories <- renderPlot({
      validate(
        need(isTruthy(rv$ccam), "Aucun acte CCAM sélectionné")
      )

      # Compter les actes par thématique
      thematique_counts <- local_rv$selected_ccam_with_thematics[, .N, by = thematique]
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
          panel.grid.major.x = element_line(
            color = "grey90",
            linetype = "dashed"
          ),
          plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt"),
          plot.background = element_rect(fill = "white", color = NA)
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
    })
  })
}

# Copy in UI
#mod_thematics_ui("thematics_1")

# Copy in server
#callModule(mod_thematics_server, "thematics_1")
