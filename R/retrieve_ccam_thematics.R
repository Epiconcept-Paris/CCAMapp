retrieve_ccam_thematics <- function(ccam, all_thematics_codes) {
   ccam_with_thematics <- lapply(
        seq_len(length(all_thematics_codes)),
        function(x) {
          if (any(ccam %in% all_thematics_codes[[x]])) {
            ccam_present_in_thematique <- intersect(
              ccam,
              all_thematics_codes[[x]]
            )
            list(
              ccam_present_in_thematique = ccam_present_in_thematique,
              thematique = names(all_thematics_codes)[x]
            )
          } else {
            return(NULL)
          }
        }
      )
      ccam_with_thematics <- data.table::rbindlist(ccam_with_thematics)
      selected_ccam_with_thematics <- data.table::data.table(code = ccam)
      if (nrow(ccam_with_thematics) > 0) {
        selected_ccam_with_thematics <- merge(
          selected_ccam_with_thematics,
          ccam_with_thematics,
          by.x = "code",
          by.y = "ccam_present_in_thematique",
          all.x = TRUE
        )
      } else {
        selected_ccam_with_thematics <- data.table::data.table(
          code = ccam,
          thematique = "Thématique inconnue"
        )
      }
      selected_ccam_with_thematics <- selected_ccam_with_thematics[
        is.na(thematique),
        thematique := "Thématique inconnue"
      ]
      thematique_levels <- unique(selected_ccam_with_thematics$thematique)
      thematique_levels <- c(
        "Thématique inconnue",
        setdiff(thematique_levels, "Thématique inconnue")
      )
      selected_ccam_with_thematics$thematique <- factor(
        selected_ccam_with_thematics$thematique,
        levels = thematique_levels
      )
      selected_ccam_with_thematics
}