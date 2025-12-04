page_sidebar(
  useShinyjs(),
  title = "Exploration actes CCAM",
  sidebar = sidebar(
    mod_ccam_select_ui("ccam1")
  ),
  tagList(
    fluidRow(
      h2("Actes CCAM sélectionnés"),
      DTOutput("selected_ccam")
    ),
    fluidRow(mod_filter_open_ccam_ui("filter_open_ccam_1")),
    fluidRow(mod_maps_ui("maps_1"))
  )
)