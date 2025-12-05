thematics_plot <- function(total_ccam_by_thematique) {
  ggplot(
        total_ccam_by_thematique,
        aes(x = reorder(thematique, ratio_swm_france), y = ratio_swm_france)
      ) +
        geom_col(fill = "steelblue", position = "dodge", width = 0.7) +
        geom_text(
          aes(label = ratio_swm_france_label),
          hjust = -0.2,
          size = 5,
          color = "black"
        ) +
        coord_flip() +
        theme_minimal() +
        xlab("") +
        ylab("% d'actes identifiés") +
        labs(title = "% d'actes identifiés par thématique",
        subtitle = "Nombre d'actes identifiés dans les établissements SWM par rapport au nombre d'actes identifiés en France") +
        theme(
          plot.title = element_text(size = 18),
          plot.subtitle = element_text(size = 14),
          axis.text.y = element_text(size = 13, hjust = 1),
          axis.text.x = element_text(size = 14),
          axis.title.x = element_text(size = 16, margin = margin(t = 10))
        )  
}