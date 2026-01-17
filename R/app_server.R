#' @keywords internal
app_server <- function(input, output, session, data = NULL) {

  dat_raw <- reactiveVal(NULL)

  # Use passed data if supplied
  observe({
    if (!is.null(data)) {
      dat_raw(fd_standardise(data))
    }
  })

  # Folder load
  observeEvent(input$load_folder, {
    df <- fd_read_folder(input$base_dir)
    validate(need(!is.null(df), "No valid CSVs in folder"))
    dat_raw(df)
  })

  # Upload load
  observeEvent(input$csv_files, {
    files <- input$csv_files$datapath
    lst   <- lapply(files, read_fd)
    lst   <- lst[!vapply(lst, is.null, logical(1))]

    validate(need(length(lst) > 0, "No valid CSVs uploaded"))

    dat_raw(dplyr::bind_rows(lst) |> fd_standardise())
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
        .data$Date     >= input$date_rng[1],
        .data$Date     <= input$date_rng[2],
        .data$Athlete  %in% input$athletes
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