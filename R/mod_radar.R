#' @keywords internal
mod_radar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        selectizeInput(
          ns("radar_metrics"), "Metrics",
          choices = NULL, multiple = TRUE,
          options = list(
            placeholder = "Pick metrics for radar (max 10)",
            maxItems    = 10
          )
        )
      )
    ),
    selectizeInput(
      ns("radar_athletes"), "Athletes",
      choices = NULL, multiple = TRUE,
      options = list(
        placeholder = "Select athletes (max 20)",
        maxItems    = 20
      )
    ),
    tags$div(
      class = "small-note",
      "Up to 10 metrics per radar and 20 athletes per view.",
      tags$br(),
      "For each athlete and metric, the radar uses the mean of all available trials in the current dataset. Values are min-max standardised across the selected athletes for each metric (0-100). 0 values may also include missing metrics.",
      tags$br(),
      "Double-click on the radar to reset the view."
    ),
    div(
      class = "card-like",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("p_radar"), height = "560px")
      )
    )
  )
}

#' @keywords internal
mod_radar_server <- function(id, base_f) {
  moduleServer(id, function(input, output, session) {
    # Initialise radar inputs
    observeEvent(base_f(),
      {
        d <- base_f()
        mets <- sort(unique(d$resultName))
        aths <- sort(unique(d$Athlete))

        if (!length(mets) || !length(aths)) {
          return(NULL)
        }

        updateSelectizeInput(
          session, "radar_metrics",
          choices = mets,
          selected = head(mets, 5)
        )

        updateSelectizeInput(
          session, "radar_athletes",
          choices = aths,
          selected = head(aths, 3)
        )
      },
      ignoreNULL = TRUE
    )

    output$p_radar <- plotly::renderPlotly({
      mets_sel <- if (is.null(input$radar_metrics)) character(0) else input$radar_metrics
      aths_sel <- if (is.null(input$radar_athletes)) character(0) else input$radar_athletes


      if (length(mets_sel) > 10) mets_sel <- head(mets_sel, 10)
      if (length(aths_sel) > 20) aths_sel <- head(aths_sel, 20)

      validate(
        need(length(mets_sel) >= 3, "Pick >3 metrics for radar"),
        need(length(aths_sel) >= 1, "Select at least one athlete")
      )

      dd <- base_f() %>%
        dplyr::filter(resultName %in% mets_sel, Athlete %in% aths_sel)

      validate(need(nrow(dd) > 0, "No data"))

      wide <- dd %>%
        dplyr::group_by(Athlete, resultName) %>%
        dplyr::summarise(val = mean(value, na.rm = TRUE), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = resultName, values_from = val)

      df <- as.data.frame(wide, check.names = FALSE)
      rownames(df) <- as.character(df$Athlete)
      df$Athlete <- NULL


      # Scale all metrics 0-100
      rng <- apply(df, 2, function(x) c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))

      scaled <- as.data.frame(mapply(
        function(x, mn, mx) {
          if (mx - mn < .Machine$double.eps) rep(50, length(x)) else 100 * (x - mn) / (mx - mn)
        },
        df, rng[1, ], rng[2, ],
        SIMPLIFY = FALSE
      ), check.names = FALSE)

      rownames(scaled) <- rownames(df)

      metrics <- colnames(scaled)
      scaled <- scaled[, metrics, drop = FALSE]
      scaled[is.na(scaled)] <- 0
      ticks <- seq(0, 100, 20)

      p <- plotly::plot_ly()

      for (ath in rownames(scaled)) {
        rvals <- as.numeric(scaled[ath, metrics])
        # Close the polygon
        rvals <- c(rvals, rvals[1])
        thetas <- c(metrics, metrics[1])

        p <- p |>
          plotly::add_trace(
            type = "scatterpolar",
            mode = "lines+markers",
            r = rvals,
            theta = thetas,
            name = ath,
            fill = "toself",
            hovertemplate = paste0(
              "Athlete: ", ath,
              "<br>Metric: %{theta}",
              "<br>Score: %{r:.1f}<extra></extra>"
            )
          )
      }

      p <- p |>
        plotly::layout(
          title = "Radar (standardised 0-100)",
          polar = list(
            radialaxis = list(range = c(0, 100), tickvals = ticks, ticktext = as.character(ticks)),
            angularaxis = list(type = "category")
          )
        )
      style_viz_plotly(p)
    })
  })
}
