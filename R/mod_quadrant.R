#' @keywords internal
mod_quadrant_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      actionButton(ns("quad_add"), "+ Add quadrant", class = "btn btn-primary"),
      actionButton(ns("quad_remove"), "\u2212 Remove", class = "btn btn-outline-secondary ms-2")
    ),
    fluidRow(
      column(
        6,
        radioButtons(
          ns("quad_agg"), "Aggregation",
          c(
            "Latest test (per athlete)"   = "latest",
            "Mean over last N days"       = "meanN",
            "Best value across all tests" = "best"
          ),
          inline = FALSE,
          selected = "latest"
        )
      ),
      column(
        6,
        conditionalPanel(
          condition = sprintf("input['%s'] == 'meanN'", ns("quad_agg")),
          sliderInput(
            ns("quad_N"), "N days (for Mean)",
            min = 3, max = 60, value = 28, step = 1
          )
        )
      )
    ),
    uiOutput(ns("quad_controls")),
    tags$div(
      class = "small-note mt-2",
      "Compare pairs of metrics per athlete. Medians define the quadrant lines."
    ),
    uiOutput(ns("quad_plots")),
    div(
      class = "mt-2",
      checkboxInput(ns("quad_show_legend"), "Show legend", value = FALSE)
    )
  )
}

#' @keywords internal
mod_quadrant_server <- function(id, base_f) {
  moduleServer(id, function(input, output, session) {
    # Wrap header if it spills
    wrap_html <- function(x, width = 40) {
      paste(strwrap(x, width = width), collapse = "<br>")
    }

    quad_count <- reactiveVal(2L)

    observeEvent(input$quad_add, {
      quad_count(quad_count() + 1L)
    })
    observeEvent(input$quad_remove, {
      quad_count(max(1L, quad_count() - 1L))
    })

    observeEvent(base_f(),
      {
        d <- base_f()
        req(nrow(d) > 0)

        mets <- sort(unique(d$resultName))
        if (!length(mets)) {
          return(NULL)
        }

        n <- quad_count()
        for (i in seq_len(n)) {
          shiny::updateSelectInput(
            session, paste0("quad_x_", i),
            choices = mets,
            selected = mets[(i - 1L) %% length(mets) + 1L]
          )
          shiny::updateSelectInput(
            session, paste0("quad_y_", i),
            choices = mets,
            selected = mets[i %% length(mets) + 1L]
          )
        }
      },
      ignoreNULL = TRUE
    )

    output$quad_controls <- renderUI({
      d <- base_f()
      req(nrow(d) > 0)

      mets <- sort(unique(d$resultName))
      n <- quad_count()
      ns <- session$ns

      tagList(lapply(seq_len(n), function(i) {
        fluidRow(
          column(
            6,
            selectInput(
              ns(paste0("quad_x_", i)),
              paste0("Quadrant ", i, " - X"),
              choices  = mets,
              selected = mets[(i - 1L) %% length(mets) + 1L]
            )
          ),
          column(
            6,
            selectInput(
              ns(paste0("quad_y_", i)),
              paste0("Quadrant ", i, " - Y"),
              choices  = mets,
              selected = mets[i %% length(mets) + 1L]
            )
          )
        )
      }))
    })

    quad_data <- reactive({
      n <- quad_count()
      mets <- character(0)

      for (i in seq_len(n)) {
        mets <- c(mets, input[[paste0("quad_x_", i)]], input[[paste0("quad_y_", i)]])
      }

      mets <- unique(stats::na.omit(mets))
      validate(need(length(mets) >= 1, "Pick metrics for quadrants"))

      dd <- base_f() %>%
        dplyr::filter(resultName %in% mets) %>%
        dplyr::group_by(Athlete, resultName, Date) %>%
        dplyr::summarise(v = safe_max(value), .groups = "drop")

      out <- switch(input$quad_agg,
        latest = dd %>%
          dplyr::group_by(Athlete, resultName) %>%
          dplyr::filter(Date == max(Date)) %>%
          dplyr::summarise(val = mean(v, na.rm = TRUE), .groups = "drop"),
        meanN = {
          cut <- max(dd$Date, na.rm = TRUE) - (input$quad_N - 1L)
          dd %>%
            dplyr::filter(Date >= cut) %>%
            dplyr::group_by(Athlete, resultName) %>%
            dplyr::summarise(val = mean(v, na.rm = TRUE), .groups = "drop")
        },
        { # best
          dd %>%
            dplyr::group_by(Athlete, resultName) %>%
            dplyr::summarise(val = safe_max(v), .groups = "drop")
        }
      )

      tidyr::pivot_wider(out, names_from = resultName, values_from = val)
    })

    # Stable athlete colour mapping
    athlete_cols <- reactive({
      df <- quad_data()
      req(nrow(df) > 0)

      lev <- sort(unique(df$Athlete))
      req(length(lev) > 0)

      cols <- grDevices::hcl.colors(length(lev), palette = "Dark 3")
      setNames(cols, lev)
    })

    output$quad_plots <- renderUI({
      n <- quad_count()
      rows <- split(seq_len(n), ceiling(seq_len(n) / 2))
      ns <- session$ns

      tagList(lapply(rows, function(idx) {
        fluidRow(lapply(idx, function(i) {
          column(
            6,
            div(
              class = "card-like",
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns(paste0("quad_plot_", i)), height = "520px")
              )
            )
          )
        }))
      }))
    })

    make_quad <- function(df, xmet, ymet, title, cols, show_legend = FALSE) {
      validate(need(
        all(c(xmet, ymet) %in% names(df)),
        paste("Missing metric:", xmet, "or", ymet)
      ))

      medx <- stats::median(df[[xmet]], na.rm = TRUE)
      medy <- stats::median(df[[ymet]], na.rm = TRUE)

      df$Athlete <- factor(df$Athlete, levels = names(cols))

      df$.hover <- paste0(
        "Athlete: ", as.character(df$Athlete),
        "<br>", xmet, ": ", signif(df[[xmet]], 4),
        "<br>", ymet, ": ", signif(df[[ymet]], 4)
      )

      ggplot2::ggplot(df, ggplot2::aes(.data[[xmet]], .data[[ymet]], text = .hover)) +
        ggplot2::geom_hline(yintercept = medy, linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
        ggplot2::geom_vline(xintercept = medx, linetype = "dashed", linewidth = 0.7, alpha = 0.8) +
        ggplot2::geom_point(ggplot2::aes(color = Athlete), size = 3, alpha = 0.9) +
        ggplot2::scale_color_manual(
          values = unname(cols),
          breaks = names(cols),
          drop = FALSE
        ) +
        ggplot2::guides(color = ggplot2::guide_legend(
          title = NULL,
          override.aes = list(size = 3, alpha = 1)
        )) +
        ggplot2::labs(title = title, x = xmet, y = ymet) +
        viz_theme() +
        ggplot2::theme(
          legend.position = if (isTRUE(show_legend)) "bottom" else "none",
          legend.text = ggplot2::element_text(size = 8),
          legend.key.size = grid::unit(0.4, "lines"),
          legend.spacing.x = grid::unit(0.2, "lines")
        )
    }

    observe({
      n <- quad_count()
      df <- quad_data()

      for (i in seq_len(n)) {
        local({
          ii <- i

          output[[paste0("quad_plot_", ii)]] <- plotly::renderPlotly({
            dff <- req(df)

            xm <- input[[paste0("quad_x_", ii)]]
            ym <- input[[paste0("quad_y_", ii)]]
            req(xm, ym)

            ttl <- paste0("Quadrant ", ii, " - ", xm, " vs ", ym)

            # Wrap long labels so they don't get clipped
            ttl_w <- wrap_html(ttl, width = 45)
            xm_w <- wrap_html(xm, width = 28)
            ym_w <- wrap_html(ym, width = 28)

            cols <- athlete_cols()
            show_leg <- isTRUE(input$quad_show_legend)

            # Build ggplot to plotly
            p <- make_quad(dff, xm, ym, ttl, cols = cols, show_legend = show_leg)
            pp <- as_viz_plotly(p, tooltip = "text")

            pp %>%
              plotly::layout(
                title = list(
                  text = paste0("<b>", ttl_w, "</b>"),
                  x = 0,
                  xanchor = "left",
                  font = list(size = 14),
                  automargin = TRUE
                ),
                xaxis = list(
                  title = list(text = paste0("<b>", xm_w, "</b>"), font = list(size = 12)),
                  automargin = TRUE
                ),
                yaxis = list(
                  title = list(text = paste0("<b>", ym_w, "</b>"), font = list(size = 12)),
                  automargin = TRUE
                ),
                showlegend = show_leg,
                legend = list(
                  traceorder = "normal",
                  orientation = "h",
                  x = 0,
                  y = -0.25,
                  title = list(text = ""),
                  font = list(size = 9),
                  itemsizing = "constant"
                ),
                margin = list(
                  t = 90, # give wrapped title room
                  b = if (show_leg) 110 else 60
                )
              )
          })
        })
      }
    })
  })
}
