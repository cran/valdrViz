#' @keywords internal
mod_longit_athlete_comparison_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        6,
        selectizeInput(
          ns("comp_athletes"), "Athletes",
          choices = NULL, multiple = TRUE
        )
      ),
      column(
        6,
        selectizeInput(
          ns("comp_metrics"), "Metrics",
          choices = NULL, multiple = TRUE
        )
      )
    ),
    div(
      class = "small-note",
      "Solid line = best-of-day. Dashed line = rolling mean (7d/14d) for the same athlete (same colour)."
    ),
    fluidRow(
      column(
        6,
        radioButtons(
          ns("comp_roll_opt"), "Rolling mean",
          choices = c("7d" = "7", "14d" = "14"),
          selected = "7",
          inline = TRUE
        )
      )
    ),
    div(
      class = "card-like",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("p_comp_facets"), height = "520px")
      )
    ),
    div(
      class = "card-like",
      h5("Trend table options"),
      checkboxInput(
        ns("trend_table_on"),
        "Show trend table (slope & p)",
        value = TRUE
      ),

      # Only show the window slider when the table is on
      conditionalPanel(
        condition = sprintf("input['%s']", ns("trend_table_on")),
        sliderInput(
          ns("trend_N"), "Trend window (days)",
          min = 7, max = 60, value = 28, step = 1
        )
      )
    ),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("trend_table_on")),
      div(
        class = "card-like",
        h5("Trend table (slope & p-value)"),
        shinycssloaders::withSpinner(
          DT::DTOutput(ns("trend_tbl"))
        ),
        div(
          class = "table-legend",
          tags$strong("How to read this table:"),
          tags$br(),
          "Athlete = athlete name",
          tags$br(),
          "Metric = ForceDecks metric name",
          tags$br(),
          "Slope (per day) = estimated change in value per day over the selected trend window",
          tags$br(),
          "p-value = statistical evidence that the slope is different from zero (smaller p suggests a clearer trend).",
          tags$br(),
          "N = number of unique test dates used in the trend window",
          tags$br(),
          "SpanDays = days between the first and last test date used"
        )
      )
    )
  )
}


#' @keywords internal
mod_longit_athlete_comparison_server <- function(id, base_f) {
  moduleServer(id, function(input, output, session) {

    # initialise compare inputs
    observeEvent(base_f(),
      {
        d <- base_f()
        mets <- sort(unique(d$resultName))
        aths <- sort(unique(d$Athlete))

        if (!length(mets) || !length(aths)) {
          return(NULL)
        }

        updateSelectizeInput(
          session, "comp_metrics",
          choices = mets,
          selected = head(mets, 3)
        )

        updateSelectizeInput(
          session, "comp_athletes",
          choices = aths,
          selected = head(aths, 2)
        )
      },
      ignoreNULL = TRUE
    )

    output$p_comp_facets <- plotly::renderPlotly({
      validate(
        need(length(input$comp_athletes) > 0, "Select athletes"),
        need(length(input$comp_metrics) > 0, "Select metrics")
      )

      dd <- base_f() %>%
        dplyr::filter(
          Athlete %in% input$comp_athletes,
          resultName %in% input$comp_metrics
        ) %>%
        dplyr::mutate(
          Date  = as.Date(Date),
          value = as.numeric(value)
        ) %>%
        dplyr::group_by(Athlete, resultName, Date) %>%
        dplyr::summarise(value = safe_max(value), .groups = "drop") %>%
        dplyr::filter(!is.na(Date), is.finite(value))

      validate(need(nrow(dd) > 0, "No data"))


      k <- as.integer(input$comp_roll_opt)
      req(!is.na(k))

      dd_roll <- dd %>%
        dplyr::arrange(Athlete, resultName, Date) %>%
        dplyr::group_by(Athlete, resultName) %>%
        dplyr::mutate(
          roll = vapply(seq_along(Date), function(j) {
            end <- Date[j]
            start <- end - (k - 1L)
            vals <- value[Date >= start & Date <= end]
            vals <- vals[!is.na(vals)]
            if (length(vals) == 0) {
              return(NA_real_)
            }
            mean(vals)
          }, numeric(1))
        ) %>%
        dplyr::ungroup()

      roll_line <- dd_roll %>%
        dplyr::filter(!is.na(roll)) %>%
        dplyr::group_by(Athlete, resultName) %>%
        dplyr::filter(dplyr::n() >= 2) %>%
        dplyr::ungroup()

      dd_plot <- dd %>%
        dplyr::transmute(Date, value, Athlete, resultName, Series = "Best-of-day")

      lt_lab <- paste0("Rolling mean (", k, "d)")

      roll_plot <- roll_line %>%
        dplyr::transmute(
          Date,
          value = roll,
          Athlete,
          resultName,
          Series = lt_lab
        )

      # For linetype legend
      lt_vals <- stats::setNames(
        c("solid", "dashed"),
        c("Best-of-day", lt_lab)
      )

      # Keep ALL points
      dd_plot_pts <- dd_plot

      # Only draw lines where the group has 2 or more points
      dd_plot_line <- dd_plot %>%
        dplyr::group_by(Athlete, resultName, Series) %>%
        dplyr::filter(dplyr::n() >= 2) %>%
        dplyr::ungroup()

      roll_plot_line <- roll_plot %>%
        dplyr::group_by(Athlete, resultName, Series) %>%
        dplyr::filter(dplyr::n() >= 2) %>%
        dplyr::ungroup()

      # Plotly is happier with POSIXct than Date
      dd_plot_pts <- dd_plot_pts %>% dplyr::mutate(DateTime = as.POSIXct(Date))
      dd_plot_line <- dd_plot_line %>% dplyr::mutate(DateTime = as.POSIXct(Date))
      roll_plot_line <- roll_plot_line %>% dplyr::mutate(DateTime = as.POSIXct(Date))

      dd_plot_pts <- dd_plot_pts %>% dplyr::filter(!is.na(.data$DateTime))
      dd_plot_line <- dd_plot_line %>% dplyr::filter(!is.na(.data$DateTime))
      roll_plot_line <- roll_plot_line %>% dplyr::filter(!is.na(.data$DateTime))


      validate(need(nrow(dd_plot_pts) > 0, "No data"))

      # Decide if points should carry the legend (when there are no line traces)
      has_lines <- (nrow(dd_plot_line) > 0 || nrow(roll_plot_line) > 0)
      show_point_leg <- !has_lines

      # Base plot - points always
      g <- ggplot2::ggplot() +
        ggplot2::geom_point(
          data = dd_plot_pts,
          ggplot2::aes(
            x = .data$DateTime, y = .data$value,
            color = Athlete,
            group = Athlete
          ),
          size = 1.8,
          show.legend = show_point_leg
        ) +
        ggplot2::facet_wrap(~resultName, scales = "free_y") +
        viz_theme() +
        ggplot2::theme(legend.position = "bottom")

      # Add best of day line only if there is line data
      if (nrow(dd_plot_line) > 0) {
        g <- g + ggplot2::geom_line(
          data = dd_plot_line,
          ggplot2::aes(
            x = .data$DateTime, y = .data$value,
            color = Athlete,
            linetype = Series,
            group = paste(Athlete, Series, sep = "__")
          ),
          linewidth = 0.9
        )
      }

      # Add rolling mean line only if there is line data
      if (nrow(roll_plot_line) > 0) {
        g <- g + ggplot2::geom_line(
          data = roll_plot_line,
          ggplot2::aes(
            x = .data$DateTime, y = .data$value,
            color = Athlete,
            linetype = Series,
            group = paste(Athlete, Series, sep = "__")
          ),
          linewidth = 1.0,
          alpha = 0.7
        )
      }

      # Only add linetype scale if any linetype-mapped layers exist
      if (has_lines) {
        g <- g + ggplot2::scale_linetype_manual(values = lt_vals)
      }

      pp <- as_viz_plotly(g, tooltip = c("x", "y", "color", "linetype"))


      # Hide marker traces in legend only when we have line traces
      for (j in seq_along(pp$x$data)) {
        mode <- pp$x$data[[j]]$mode
        if (!show_point_leg && !is.null(mode) && grepl("markers", mode)) {
          pp$x$data[[j]]$showlegend <- FALSE
        }
      }

      # De-duplicate legend items
      seen <- character(0)
      for (j in seq_along(pp$x$data)) {
        mode <- pp$x$data[[j]]$mode
        if (!show_point_leg && !is.null(mode) && grepl("markers", mode)) next

        nm <- pp$x$data[[j]]$name
        if (is.null(nm)) next

        nm2 <- sub(",\\s*\\d+$", "", nm)
        pp$x$data[[j]]$name <- nm2

        if (nm2 %in% seen) {
          pp$x$data[[j]]$showlegend <- FALSE
        } else {
          pp$x$data[[j]]$showlegend <- TRUE
          seen <- c(seen, nm2)
        }
      }

      pp
    })

    output$trend_tbl <- DT::renderDT({
      req(input$trend_table_on)

      dd <- base_f() %>%
        dplyr::filter(
          Athlete %in% input$comp_athletes,
          resultName %in% input$comp_metrics
        ) %>%
        dplyr::mutate(
          Date  = as.Date(Date),
          value = as.numeric(value)
        ) %>%
        dplyr::group_by(Athlete, resultName, Date) %>%
        dplyr::summarise(value = safe_max(value), .groups = "drop") %>%
        dplyr::filter(!is.na(Date), is.finite(value))

      validate(need(nrow(dd) > 0, "No data"))

      dates <- dd$Date[!is.na(dd$Date)]
      validate(need(length(dates) > 0, "No valid dates"))

      end <- max(dates)
      cut <- end - (input$trend_N - 1)

      d2 <- dd %>% dplyr::filter(Date >= cut)
      validate(need(nrow(d2) > 0, "No data in selected trend window"))


      trend <- d2 %>%
        dplyr::group_by(Athlete, resultName) %>%
        dplyr::group_modify(~ {
          df <- .x %>% dplyr::arrange(Date)

          n_dates <- dplyr::n_distinct(df$Date)
          span_days <- if (all(is.na(df$Date))) {
            NA_integer_
          } else {
            as.integer(max(df$Date, na.rm = TRUE) - min(df$Date, na.rm = TRUE))
          }

          if (n_dates < 3) {
            return(tibble::tibble(
              slope = NA_real_, p = NA_real_, N = n_dates, SpanDays = span_days
            ))
          }

          if (stats::sd(df$value, na.rm = TRUE) == 0) {
            return(tibble::tibble(
              slope = 0, p = 1, N = n_dates, SpanDays = span_days
            ))
          }

          fit <- tryCatch(stats::lm(value ~ as.numeric(Date), data = df), error = function(e) NULL)
          if (is.null(fit)) {
            return(tibble::tibble(
              slope = NA_real_, p = NA_real_, N = n_dates, SpanDays = span_days
            ))
          }

          sm <- summary(fit)
          tibble::tibble(
            slope = stats::coef(sm)[2, 1],
            p = stats::coef(sm)[2, 4],
            N = n_dates,
            SpanDays = span_days
          )
        })

      tab <- trend %>%
        dplyr::arrange(resultName, Athlete) %>%
        dplyr::mutate(
          slope = round(slope, 2),
          p     = round(p, 3)
        ) %>%
        dplyr::rename(
          Metric = resultName,
          `Slope (per day)` = slope,
          `p-value` = p
        )

      tab <- tab %>%
        dplyr::mutate(
          `Slope (per day)` = dplyr::if_else(is.na(`Slope (per day)`), "-", sprintf("%.2f", `Slope (per day)`)),
          `p-value`         = dplyr::if_else(is.na(`p-value`), "-", sprintf("%.3f", `p-value`))
        )


      DT::datatable(
        tab,
        options  = list(pageLength = 12, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
