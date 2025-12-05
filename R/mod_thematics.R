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
          plotOutput(ns("ratio_SWM_France_by_categories"), height = "100%")
        )
      )
    )
  )
}

mod_thematics_server <- function(id, rv, all_thematics_codes) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    local_rv <- reactiveValues(
      selected_ccam_with_thematics = NULL,
      ratio_swm_france_by_thematique = NULL
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
      selected_ccam_with_thematics <- retrieve_ccam_thematics(
        rv$ccam,
        all_thematics_codes
      )

      local_rv$selected_ccam_with_thematics <- selected_ccam_with_thematics[order(
        code
      )]
    })

    output$ratio_SWM_France_by_categories <- renderPlot({
      validate(
        need(isTruthy(rv$ccam), "Aucun acte CCAM sélectionné")
      )
      req(local_rv$selected_ccam_with_thematics)
      req(rv$stats_swm_selected_ccam_by_act_code)
      req(rv$stats_nationales_selected_ccam_by_act_code)

      selected_ccam_with_thematics_with_swm_counts <- merge(
        local_rv$selected_ccam_with_thematics,
        rv$stats_swm_selected_ccam_by_act_code,
        by.x = "code",
        by.y = "acte",
        all.x = TRUE
      ) %>%
        rename(total_nb_actes_swm = total_nb_actes)

      selected_ccam_with_thematics_with_france_and_swm_counts <- merge(
        selected_ccam_with_thematics_with_swm_counts,
        rv$stats_nationales_selected_ccam_by_act_code,
        by.x = "code",
        by.y = "acte",
        all.x = TRUE
      ) %>%
        rename(total_nb_actes_france = total_nb_actes)

      total_ccam_by_thematique <- selected_ccam_with_thematics_with_france_and_swm_counts[,
        .(
          total_nb_actes_france = sum(total_nb_actes_france, na.rm = TRUE),
          total_nb_actes_swm = sum(total_nb_actes_swm, na.rm = TRUE)
        ),
        by = thematique
      ] %>%
        .[, ratio_swm_france := if_else(total_nb_actes_france == 0, 0, total_nb_actes_swm / total_nb_actes_france)] %>%
        .[,
          ratio_swm_france_label := if_else(
            total_nb_actes_france == 0,
            "Thématique non trouvée",
            paste0(round(ratio_swm_france * 100), "%")
          )
        ] %>%
        .[order(thematique)]

      validate(
        need(!all(total_ccam_by_thematique$thematique %in% "Thématique inconnue"), "Aucune thématique trouvée pour les actes sélectionnés")
      )
      thematics_plot(total_ccam_by_thematique)
    })
  })

  # output$selected_ccam_with_categories <- renderPlot({
  #   validate(
  #     need(isTruthy(rv$ccam), "Aucun acte CCAM sélectionné")
  #   )

  #       selected_ccam_with_thematics <- retrieve_ccam_thematics(rv$ccam, all_thematics_codes)

  #   local_rv$selected_ccam_with_thematics <- selected_ccam_with_thematics[order(
  #     thematique
  #   )]

  #   # Compter les actes par thématique
  #   thematique_counts <- local_rv$selected_ccam_with_thematics[, .N, by = thematique]
  #   thematique_counts <- thematique_counts[order(-N)]

  #   ggplot(thematique_counts, aes(x = reorder(thematique, N), y = N)) +
  #     geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  #     geom_text(aes(label = N), hjust = -0.2, size = 5, color = "black") +
  #     coord_flip() +
  #     theme_minimal() +
  #     xlab("") +
  #     ylab("Nombre d'actes CCAM") +
  #     theme(
  #       axis.text.y = element_text(size = 13, hjust = 1),
  #       axis.text.x = element_text(size = 14),
  #       axis.title.x = element_text(size = 16, margin = margin(t = 10)),
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       panel.grid.major.x = element_line(
  #         color = "grey90",
  #         linetype = "dashed"
  #       ),
  #       plot.margin = margin(t = 20, r = 60, b = 20, l = 20, unit = "pt"),
  #       plot.background = element_rect(fill = "white", color = NA)
  #     ) +
  #     scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  # })
  #   })
}

# Copy in UI
#mod_thematics_ui("thematics_1")

# Copy in server
#callModule(mod_thematics_server, "thematics_1")
