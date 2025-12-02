#' Initialiser la connexion DuckDB et charger le référentiel d'actes
#'
#' @return Connexion DuckDB
#' @import duckdb
#' @import DBI
#' @noRd
init_duckdb <- function() {
  # Créer une connexion DuckDB en mémoire
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  
  # Chemin vers le fichier CSV
  csv_path <- app_sys("referentiel_actes.csv")
  
  # Lire le CSV avec R pour gérer correctement l'encodage UTF-8
  # puis charger dans DuckDB
  referentiel_data <- read.csv(
    file = csv_path,
    encoding = "UTF-8",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  # S'assurer que les colonnes ont les bons noms
  colnames(referentiel_data) <- c("COD_ACTE", "DT_MODIF", "NOM_LONG")
  
  # Charger les données dans DuckDB
  DBI::dbWriteTable(
    conn = con,
    name = "referentiel_actes",
    value = referentiel_data,
    overwrite = TRUE
  )
  
  # Note: DuckDB n'a pas besoin d'index explicites pour les recherches LIKE
  # Il optimise automatiquement les requêtes
  
  return(con)
}

#' Fermer la connexion DuckDB
#'
#' @param con Connexion DuckDB
#' @import DBI
#' @noRd
close_duckdb <- function(con) {
  if (!is.null(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}

