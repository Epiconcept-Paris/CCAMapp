#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialiser la connexion DuckDB
  con_duckdb <- init_duckdb()
  
  # Nettoyer la connexion à la fermeture de l'application
  onStop(function() {
    close_duckdb(con_duckdb)
  })
  
  # Module de recherche d'actes
  selected_actes <- mod_acte_search_server("acte_search_1", con_duckdb)
  
  # Observer pour réagir aux actes sélectionnés
  observe({
    actes <- selected_actes()
    if (!is.null(actes) && length(actes) > 0) {
      # Ici vous pouvez ajouter la logique pour visualiser les actes sélectionnés
      # Par exemple, récupérer les données et créer des graphiques
    }
  })
}
