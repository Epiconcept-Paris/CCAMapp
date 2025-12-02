#' Module UI pour la recherche d'actes CCAM
#'
#' @param id Module id
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @noRd
mod_acte_search_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    div(
      class = "acte-search-container",
      selectizeInput(
        inputId = ns("acte_search"),
        label = "Rechercher un acte CCAM",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = "Tapez un code CCAM ou un libellé (minimum 2 caractères)...",
          maxOptions = 50,
          maxItems = NULL,
          create = FALSE,
          persist = FALSE,
          render = I("{
            option: function(item, escape) {
              // item.value est le code, item.label est 'code - libellé'
              var parts = item.label.split(' - ');
              var code = parts[0] || item.value;
              var libelle = parts.slice(1).join(' - ') || '';
              return '<div style=\"padding: 5px;\">' +
                '<strong style=\"color: #0066cc;\">' + escape(code) + '</strong>' +
                (libelle ? '<br/><span style=\"color: #666; font-size: 0.9em;\">' + escape(libelle) + '</span>' : '') +
                '</div>';
            },
            item: function(item, escape) {
              // item.value est le code, item.label est 'code - libellé'
              return '<div style=\"padding: 5px;\">' +
                escape(item.label) +
                '</div>';
            }
          }")
        )
      ),
      uiOutput(ns("selected_actes_info"))
    )
  )
}

#' Module Server pour la recherche d'actes CCAM
#'
#' @param id Module id
#' @param con_connexion DuckDB connection
#' @import shiny
#' @import DBI
#' @noRd
mod_acte_search_server <- function(id, con_connexion) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Fonction pour rechercher les actes dans DuckDB
    search_actes_db <- function(search_term) {
      if (is.null(search_term) || search_term == "" || nchar(trimws(search_term)) < 2) {
        return(data.frame(COD_ACTE = character(0), NOM_LONG = character(0)))
      }
      
      search_term_clean <- trimws(search_term)
      search_term_escaped <- gsub("'", "''", search_term_clean)  # Échapper les apostrophes
      
      # Requête DuckDB pour rechercher dans le code ou le libellé
      query <- paste0(
        "SELECT DISTINCT COD_ACTE, NOM_LONG ",
        "FROM referentiel_actes ",
        "WHERE UPPER(COD_ACTE) LIKE UPPER('%", search_term_escaped, "%') ",
        "OR UPPER(NOM_LONG) LIKE UPPER('%", search_term_escaped, "%') ",
        "ORDER BY ",
        "CASE ",
        "WHEN UPPER(COD_ACTE) = UPPER('", search_term_escaped, "') THEN 1 ",
        "WHEN UPPER(COD_ACTE) LIKE UPPER('", search_term_escaped, "%') THEN 2 ",
        "WHEN UPPER(NOM_LONG) LIKE UPPER('", search_term_escaped, "%') THEN 3 ",
        "WHEN UPPER(COD_ACTE) LIKE UPPER('%", search_term_escaped, "%') THEN 4 ",
        "ELSE 5 END, ",
        "COD_ACTE ",
        "LIMIT 50"
      )
      
      tryCatch({
        result <- DBI::dbGetQuery(con_connexion, query)
        return(result)
      }, error = function(e) {
        showNotification(
          paste("Erreur lors de la recherche:", e$message),
          type = "error",
          duration = 5
        )
        return(data.frame(COD_ACTE = character(0), NOM_LONG = character(0)))
      })
    }
    
    # Variable réactive pour stocker le dernier terme de recherche
    last_search <- reactiveVal("")
    
    # Input réactif pour le terme de recherche (mis à jour par JavaScript avec debounce)
    search_term_input <- reactive({
      input$search_term_js
    })
    
    # Debounce du terme de recherche
    search_term_debounced <- debounce(search_term_input, millis = 300)
    
    # Observer pour mettre à jour les choix quand le terme de recherche change
    observe({
      search_term <- search_term_debounced()
      
      if (is.null(search_term) || search_term == "" || nchar(trimws(search_term)) < 2) {
        updateSelectizeInput(
          session = session,
          inputId = "acte_search",
          choices = list(),
          server = TRUE
        )
        last_search("")
        return()
      }
      
      # Éviter de rechercher le même terme plusieurs fois
      if (search_term == last_search()) {
        return()
      }
      last_search(search_term)
      
      result <- search_actes_db(search_term)
      
      if (nrow(result) > 0) {
        # Créer une liste nommée pour les choix
        # Format: list(code = "code - libellé")
        # Le code est la valeur, le libellé est utilisé pour l'affichage
        choices_list <- setNames(
          as.list(result$COD_ACTE),
          paste0(result$COD_ACTE, " - ", result$NOM_LONG)
        )
        
        updateSelectizeInput(
          session = session,
          inputId = "acte_search",
          choices = choices_list,
          server = TRUE
        )
      } else {
        updateSelectizeInput(
          session = session,
          inputId = "acte_search",
          choices = list(),
          server = TRUE
        )
      }
    })
    
    # JavaScript pour capturer les changements dans selectizeInput avec debounce
    # On attend que selectizeInput soit initialisé
    observe({
      # Attendre que l'élément soit prêt
      shinyjs::runjs(paste0("
        (function() {
          var initInterval = setInterval(function() {
            var $select = $('#", ns("acte_search"), "');
            if ($select.length && $select.data('selectize')) {
              clearInterval(initInterval);
              
              var selectize = $select[0].selectize;
              var timeout;
              var lastValue = '';
              
              // Écouter les changements dans le champ de recherche
              selectize.on('type', function(value) {
                clearTimeout(timeout);
                
                if (value.length < 2) {
                  Shiny.setInputValue('", ns("search_term_js"), "', '', {priority: 'event'});
                  lastValue = '';
                  return;
                }
                
                if (value === lastValue) {
                  return;
                }
                
                lastValue = value;
                
                timeout = setTimeout(function() {
                  Shiny.setInputValue('", ns("search_term_js"), "', value, {priority: 'event'});
                }, 300);
              });
              
              // Réinitialiser quand le champ est vidé
              selectize.on('clear', function() {
                clearTimeout(timeout);
                lastValue = '';
                Shiny.setInputValue('", ns("search_term_js"), "', '', {priority: 'event'});
              });
            }
          }, 100);
          
          // Arrêter après 5 secondes si l'élément n'est pas trouvé
          setTimeout(function() {
            clearInterval(initInterval);
          }, 5000);
        })();
      "))
    })
    
    # Afficher le nombre d'actes sélectionnés
    output$selected_actes_info <- renderUI({
      selected <- input$acte_search
      if (!is.null(selected) && length(selected) > 0) {
        # Les valeurs sélectionnées sont directement les codes
        codes <- selected
        
        div(
          style = "margin-top: 10px; font-size: 0.9em;",
          strong(paste0(length(codes), " acte(s) sélectionné(s)")),
          br(),
          tags$small(
            style = "color: #666;",
            paste(codes, collapse = ", ")
          )
        )
      } else {
        div(
          style = "margin-top: 10px; font-size: 0.9em; color: #666;",
          "Aucun acte sélectionné"
        )
      }
    })
    
    # Retourner les actes sélectionnés (codes uniquement)
    selected_actes <- reactive({
      selected <- input$acte_search
      if (is.null(selected) || length(selected) == 0) {
        return(character(0))
      }
      
      # Les valeurs sont déjà les codes
      return(selected)
    })
    
    return(selected_actes)
  })
}
