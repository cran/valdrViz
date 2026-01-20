#' @keywords internal
app_server <- function(input, output, session, data = NULL) {
  MAX_MB <- 250

  dat_raw <- reactiveVal(NULL)

  # Use passed data if supplied
  observe({
    if (!is.null(data)) {
      dat_raw(fd_standardise(data))
    }
  })

  # Folder load
  observeEvent(input$load_folder, {
    df <- fd_read_folder(input$base_dir, max_mb = MAX_MB)

    if (is.null(df)) {
      shiny::showNotification(
        "No valid CSVs in folder (check size/format/required columns).",
        type = "error",
        duration = 8
      )
      return(NULL)
    }

    dat_raw(df)
  })

  # Upload load
  observeEvent(input$csv_files, {
    req(input$csv_files)

    files <- input$csv_files$datapath
    sizes <- input$csv_files$size

    if (any(sizes > MAX_MB * 1024^2)) {
      shiny::showNotification(
        paste0("One or more files exceed ", MAX_MB, " MB."),
        type = "error",
        duration = 8
      )
      return(NULL)
    }

    lst_all <- lapply(files, read_fd, max_mb = MAX_MB)
    ok <- !vapply(lst_all, is.null, logical(1))
    lst <- lst_all[ok]

    if (length(lst) == 0) {
      shiny::showNotification(
        "No valid CSVs uploaded (check format/required columns).",
        type = "error",
        duration = 8
      )
      return(NULL)
    }

    if (any(!ok)) {
      shiny::showNotification(
        "Some files were skipped because they were invalid or too large.",
        type = "warning",
        duration = 6
      )
    }

    dat_raw(fd_standardise(dplyr::bind_rows(lst)))
  })

  dat <- reactive({
    req(dat_raw())
    dat_raw()
  })

  # Shared filtered data for all modules
  base_f <- reactive({
    d <- dat()
    req(d, input$testtype, input$athletes, input$date_rng)

    d %>%
      dplyr::filter(
        .data$testType == input$testtype,
        .data$Date >= input$date_rng[1],
        .data$Date <= input$date_rng[2],
        .data$Athlete %in% input$athletes
      )
  })

  # Initialise only the sidebar inputs
  observeEvent(dat(), {
    d <- dat()
    init_main_inputs(session, d)
  })

  # Call modules
  mod_metric_explorer_server("metric_explorer", base_f = base_f)
  mod_radar_server("radar", base_f = base_f)
  mod_longit_athlete_comparison_server("longit", base_f = base_f)
  mod_quadrant_server("quadrant", base_f = base_f)
  mod_player_report_server("player_report", base_f = base_f)
}
