#' ccam_select UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput selectizeInput actionButton debounce reactive observeEvent updateSelectizeInput updateTextInput updateSelectInput moduleServer reactiveVal
#' @import shinyjs
mod_ccam_select_ui <- function(id) {
  ns <- NS(id)

  tagList(
    actionButton(
      ns("erase_selection"),
      "Effacer la sélection",
      class = "btn-danger"
    ),
    radioButtons(
      inputId = ns("search_type"),
      label = "Type de recherche",
      choices = c("Code/Libellé" = "code_libelle", "Thématique" = "thematique"),
      selected = "thematique"
    ),
    div(
      id = ns("code_libelle_container"),
      style = "display: none;",
      h4("Recherche d'actes CCAM par code ou libellé"),
      textInput(
        inputId = ns("search"),
        label = "Recherche (code ou libellé)",
        placeholder = "Ex : radio, AAQP…"
      ),
      div(
        id = ns("ccam_container"),
        style = "display: none;",
        h5("Faites votre sélection d'actes CCAM"),
        selectizeInput(
          inputId = ns("ccam"),
          label = "Actes CCAM trouvés (max 25 résultats)",
          choices = NULL,
          multiple = TRUE,
          options = list(
            maxOptions = 25,
            closeAfterSelect = FALSE
          )
        ),
        actionButton(
          inputId = ns("select_all"),
          label = "Sélectionner tous les résultats proposés"
        )
      )
    ),
    div(
      id = ns("thematique_container"),
      h4("Recherche d'actes CCAM par thématique"),
      selectInput(
        inputId = ns("select_ccam_theme"),
        label = "Sélectionner des actes par thématique",
        choices = NULL,
        multiple = TRUE
      )
    )
  )
}

#' ccam_select Server Functions
#'
#' @noRd
#' @importFrom dplyr collect filter %>% select
#'
mod_ccam_select_server <- function(
  id,
  referentiel_actes_csv,
  rv,
  all_thematics_codes,
  limit = 25
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$search_type, {
      if (input$search_type == "code_libelle") {
        shinyjs::runjs(sprintf(
          '$("%s").show()',
          paste0("#", ns("code_libelle_container"))
        ))
        shinyjs::runjs(sprintf(
          '$("%s").hide()',
          paste0("#", ns("thematique_container"))
        ))
      } else {
        shinyjs::runjs(sprintf(
          '$("%s").show()',
          paste0("#", ns("thematique_container"))
        ))
        shinyjs::runjs(sprintf(
          '$("%s").hide()',
          paste0("#", ns("code_libelle_container"))
        ))
      }
    })

    local_rv <- reactiveValues(
      ccam_thematique = NULL,
      ccam = NULL
    )

    updateSelectInput(
      session,
      "select_ccam_theme",
      choices = names(all_thematics_codes)
    )

    observeEvent(input$select_ccam_theme, {
      req(input$select_ccam_theme)

      index_thematique <- which(
        names(all_thematics_codes) %in% input$select_ccam_theme
      )

      ccam_thematique <- unlist(all_thematics_codes[index_thematique])

      local_rv$ccam_thematique <- ccam_thematique
    })

    # Variable réactive pour stocker les choix disponibles
    current_choices <- reactiveVal(NULL)

    search_term <- debounce(reactive(input$search), 200)

    observeEvent(search_term(), {
      req(nchar(search_term()) >= 2)
      shinyjs::runjs(sprintf(
        '$("%s").hide()',
        paste0("#", ns("ccam_container"))
      ))

      res <- referentiel_actes_csv[
        COD_ACTE %like% search_term() | NOM_COURT %like% search_term(),
        .(COD_ACTE, NOM_COURT)
      ][1:limit]
      res <- res[order(COD_ACTE)]

      choices <- setNames(res$COD_ACTE, paste(res$COD_ACTE, "-", res$NOM_COURT))
      current_choices(choices)

      updateSelectizeInput(
        session,
        "ccam",
        choices = choices,
        server = TRUE
      )

      shinyjs::runjs(sprintf(
        '$("%s").show()',
        paste0("#", ns("ccam_container"))
      ))
    })

    observeEvent(input$ccam, {
      local_rv$ccam <- c(local_rv$ccam, input$ccam)
    })

    observeEvent(c(local_rv$ccam, local_rv$ccam_thematique), {
      rv$ccam <- unique(c(local_rv$ccam, local_rv$ccam_thematique))
    })

    observeEvent(rv$ccam, {
      rv$filtered_referentiel <- referentiel_actes_csv[
        COD_ACTE %in% rv$ccam,
        .(COD_ACTE, NOM_COURT)
      ]
    })

    # Gestion du bouton "Select All"
    observeEvent(input$select_all, {
      choices <- current_choices()
      if (!is.null(choices) && length(choices) > 0) {
        updateSelectizeInput(
          session,
          "ccam",
          selected = as.character(choices)
        )
      }
    })

    observeEvent(input$erase_selection, {
      rv$ccam <- NULL
      rv$filtered_referentiel <- NULL
      rv$swm_etablissements_with_selected_ccam <- NULL
      local_rv$ccam_thematique <- NULL
      local_rv$ccam <- NULL

      updateTextInput(
        session,
        "search",
        value = ""
      )

      updateSelectizeInput(
        session,
        "ccam",
        choices = NULL,
        selected = NULL,
        server = TRUE
      )

      # On utilise JavaScript pour forcer la réinitialisation de la sélection
      # Car j'avais des problèmes avec le selectizeInput lors de la réinitialisation
      # shinyjs::runjs(sprintf(
      #   'setTimeout(function() {
      #      var $el = $("#%s");
      #      if ($el.length && $el[0].selectize) {
      #        var selectize = $el[0].selectize;
      #        selectize.clear();
      #        selectize.clearOptions();
      #      }
      #    }, 100);',
      #   ns("ccam")
      # ))

      shinyjs::runjs(sprintf(
        '$("%s").hide()',
        paste0("#", ns("ccam_container"))
      ))

      updateSelectInput(
        session,
        "select_ccam_theme",
        choices = names(all_thematics_codes),
        selected = NULL
      )

      current_choices(NULL)
    })
  })
}

## To be copied in the UI
# mod_ccam_select_ui("ccam_select_1")

## To be copied in the server
# mod_ccam_select_server("ccam_select_1")
