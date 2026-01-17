#' @keywords internal
mod_player_report_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        3,
        selectInput(
          ns("rep_ath"), "Athlete",
          choices = NULL
        )
      ),
      column(
        9,
        selectizeInput(
          ns("rep_metrics"), "Metrics",
          choices = NULL, multiple = TRUE,
          options = list(
            maxItems    = 12,
            placeholder = "Select metrics to include"
          )
        )
      )
    ),
    div(
      class = "card-like",
      shinycssloaders::withSpinner(
        uiOutput(ns("p_player_report_ui"))
      )
    ),
    fluidRow(
      column(
        6,
        div(
          class = "card-like",
          h5("Recent form (14d vs 28d)"),
          DT::DTOutput(ns("rep_form_tbl")),
          div(
            class = "table-legend",
            tags$strong("How to read this table:"),
            tags$br(),
            "Mean 14d = average over last 14 days",
            tags$br(),
            "Mean 28d = average over last 28 days",
            tags$br(),
            "Change 14 vs 28 (%) = percentage difference between the 14-day mean and the 28-day mean"
          )
        )
      ),
      column(
        6,
        div(
          class = "card-like",
          h5("Stability (CV%, TE)"),
          DT::DTOutput(ns("rep_stab_tbl")),
          div(
            class = "table-legend",
            tags$strong("How to read this table:"),
            tags$br(),
            "Days With Tests = number of days with data for this metric",
            tags$br(),
            "Mean = average value across those test days",
            tags$br(),
            "SD = standard deviation of values across test days",
            tags$br(),
            "CV (%) = coefficient of variation (SD / Mean x 100)",
            tags$br(),
            "TE = typical error (SD / sqrt(2))"
          )
        )
      )
    ),
    div(
      class = "card-like",
      downloadButton(
        ns("dl_player_pdf"),
        "\u2B07\uFE0F Download Player Report (PDF)"
      )
    )
  )
}

#' @keywords internal
mod_player_report_server <- function(id, base_f) {
  moduleServer(id, function(input, output, session) {
    # Initialise player report inputs
    observeEvent(base_f(),
      {
        d <- base_f()
        mets <- sort(unique(d$resultName))
        aths <- sort(unique(d$Athlete))

        if (!length(mets) || !length(aths)) {
          return(NULL)
        }

        updateSelectInput(
          session, "rep_ath",
          choices = aths,
          selected = aths[1]
        )

        updateSelectizeInput(
          session, "rep_metrics",
          choices = mets
        )
      },
      ignoreNULL = TRUE
    )

    pr_facet_height <- reactive({
      n_mets <- length(input$rep_metrics %||% character(0))
      n_mets <- max(1, min(12, n_mets))

      ncol <- 2
      nrow <- max(1, ceiling(n_mets / ncol))

      base <- 360
      per_row <- 260

      max(520, base + per_row * nrow)
    })

    output$p_player_report_ui <- renderUI({
      plotly::plotlyOutput(
        session$ns("p_player_report"),
        height = paste0(pr_facet_height(), "px")
      )
    })

    # Base data for selected athlete and metrics
    pr_filtered <- reactive({
      d <- base_f()
      req(nrow(d) > 0, input$rep_ath, input$rep_metrics)

      d %>%
        dplyr::filter(
          Athlete == input$rep_ath,
          resultName %in% input$rep_metrics
        )
    })

    # Time-series panel
    output$p_player_report <- plotly::renderPlotly({
      b <- pr_filtered()

      ath <- b %>%
        dplyr::group_by(resultName, Date) %>%
        dplyr::summarise(v = mean(value, na.rm = TRUE), .groups = "drop")

      validate(need(nrow(ath) > 0, "No data for chosen metrics."))

      g <- ggplot2::ggplot(ath, ggplot2::aes(Date, v, color = resultName)) +
        ggplot2::geom_line(linewidth = 1.1) +
        ggplot2::geom_point(size = 2.2) +
        ggplot2::facet_wrap(~resultName, scales = "free_y", ncol = 2) +
        ggplot2::labs(
          title = paste("Player Report -", input$rep_ath),
          y     = "Value",
          color = "Metric"
        ) +
        viz_theme() +
        ggplot2::theme(legend.position = "bottom")

      as_viz_plotly(g, tooltip = c("x", "y", "color"))
    })

    # Recent form table (14d vs 28d)
    output$rep_form_tbl <- DT::renderDT({
      s <- pr_filtered() %>%
        dplyr::group_by(resultName, Date) %>%
        dplyr::summarise(v = mean(value, na.rm = TRUE), .groups = "drop")

      validate(need(nrow(s) > 0, "No data."))

      end <- max(s$Date, na.rm = TRUE)

      s14 <- s %>%
        dplyr::filter(Date > end - 14) %>%
        dplyr::group_by(resultName) %>%
        dplyr::summarise(mean14 = mean(v), .groups = "drop")

      s28 <- s %>%
        dplyr::filter(Date > end - 28) %>%
        dplyr::group_by(resultName) %>%
        dplyr::summarise(mean28 = mean(v), .groups = "drop")

      tab <- dplyr::left_join(s14, s28, by = "resultName") %>%
        dplyr::mutate(
          delta_perc = 100 * (mean14 - mean28) /
            pmax(mean28, .Machine$double.eps)
        ) %>%
        dplyr::mutate(
          mean14     = round(mean14, 2),
          mean28     = round(mean28, 2),
          delta_perc = round(delta_perc, 2)
        ) %>%
        dplyr::rename(
          Metric                = resultName,
          `Mean 14d`            = mean14,
          `Mean 28d`            = mean28,
          `Change 14 vs 28 (%)` = delta_perc
        )

      DT::datatable(
        tab,
        options  = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Stability table (CV%, TE)
    output$rep_stab_tbl <- DT::renderDT({
      s <- pr_filtered() %>%
        dplyr::group_by(resultName, Date) %>%
        dplyr::summarise(v = mean(value, na.rm = TRUE), .groups = "drop")

      validate(need(nrow(s) > 0, "No data."))

      tab <- s %>%
        dplyr::group_by(resultName) %>%
        dplyr::summarise(
          days = dplyr::n(),
          mean = mean(v),
          sd = stats::sd(v),
          CV = 100 * sd / pmax(abs(mean), .Machine$double.eps),
          TE = sd / sqrt(2),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          mean = round(mean, 2),
          sd   = round(sd, 2),
          CV   = round(CV, 2),
          TE   = round(TE, 2)
        ) %>%
        dplyr::rename(
          Metric = resultName,
          `Days With Tests` = days,
          `Mean` = mean,
          `SD` = sd,
          `CV (%)` = CV,
          `TE` = TE
        )

      DT::datatable(
        tab,
        options  = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # PDF download
    output$dl_player_pdf <- downloadHandler(
      filename = function() {
        paste0(
          "player_report_",
          gsub(" ", "_", tolower(input$rep_ath)),
          ".pdf"
        )
      },
      content = function(file) {
        # Progress modal shown only for this download
        shiny::withProgress(
          message = "Generating Player Report (PDF)...",
          value = 0,
          {
            # 1 - Bail early if deps arent installed
            shiny::incProgress(0.05, detail = "Checking packages")
            if (!requireNamespace("rmarkdown", quietly = TRUE) ||
              !requireNamespace("knitr", quietly = TRUE)) {
              stop(
                "PDF export requires the 'rmarkdown' and 'knitr' packages. Please install them and retry.",
                call. = FALSE
              )
            }

            # 2 - Normal logic if we got past the check
            shiny::incProgress(0.20, detail = "Preparing data")
            b <- pr_filtered()

            dd <- b %>%
              dplyr::group_by(resultName, Date) %>%
              dplyr::summarise(v = mean(value, na.rm = TRUE), .groups = "drop")

            if (nrow(dd) == 0) {
              stop("No data for report.", call. = FALSE)
            }

            end <- max(dd$Date, na.rm = TRUE)

            # 3 - Build tables for the report
            shiny::incProgress(0.40, detail = "Building summary tables")
            form_tab <- dd %>%
              dplyr::group_by(resultName) %>%
              dplyr::summarise(
                mean14 = mean(v[Date > end - 14], na.rm = TRUE),
                mean28 = mean(v[Date > end - 28], na.rm = TRUE),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                delta_perc = 100 * (mean14 - mean28) /
                  pmax(mean28, .Machine$double.eps)
              ) %>%
              dplyr::mutate(
                mean14     = round(mean14, 2),
                mean28     = round(mean28, 2),
                delta_perc = round(delta_perc, 2)
              ) %>%
              dplyr::rename(
                Metric                = resultName,
                `Mean 14d`            = mean14,
                `Mean 28d`            = mean28,
                `Change 14 vs 28 (%)` = delta_perc
              )

            stab_tab <- dd %>%
              dplyr::group_by(resultName) %>%
              dplyr::summarise(
                days = dplyr::n(),
                mean = mean(v),
                sd = stats::sd(v),
                CV = 100 * sd / pmax(abs(mean), .Machine$double.eps),
                TE = sd / sqrt(2),
                .groups = "drop"
              ) %>%
              dplyr::mutate(
                mean = round(mean, 2),
                sd   = round(sd, 2),
                CV   = round(CV, 2),
                TE   = round(TE, 2)
              ) %>%
              dplyr::rename(
                Metric = resultName,
                `Days With Tests` = days,
                `Mean` = mean,
                `SD` = sd,
                `CV (%)` = CV,
                `TE` = TE
              )

            # 4 - Write Rmd template + render to HTML
            shiny::incProgress(0.55, detail = "Writing report template")
            rmd <- tempfile(fileext = ".Rmd")
            html_out <- tempfile(fileext = ".html")

            writeLines(c(
              "---",
              "title: ''",
              "output: html_document",
              "params:",
              "  dd: NULL",
              "  form: NULL",
              "  stab: NULL",
              "  athlete: ''",
              "---",
              "<style>",
              "  @page { size: A4 landscape; }",
              "  body { font-family: Roboto, -apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif; }",
              "</style>",
              "```{r setup, include=FALSE}",
              "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
              "library(ggplot2)",
              "library(dplyr)",
              "```",
              "# Player Report",
              "",
              "**Athlete:** `r params$athlete`",
              "",
              "```{r metrics, results='asis', fig.width=11, fig.height=7}",
              "mets <- unique(params$dd$resultName)",
              "for (i in seq_along(mets)) {",
              "  m <- mets[i]",
              "  cat('<p><strong>Metric: ', m, '</strong></p>\\n', sep = '')",
              "",
              "  p <- ggplot(dplyr::filter(params$dd, resultName == m),",
              "              aes(Date, v, color = resultName)) +",
              "    geom_line(linewidth = 1.1) +",
              "    geom_point(size = 2.2) +",
              "    labs(y = 'Value', color = 'Metric') +",
              "    valdrViz:::viz_theme()",
              "",
              "  print(p)",
              "",
              "  if (i < length(mets)) {",
              "    cat('<div style=\"page-break-after: always;\"></div>\\n')",
              "  }",
              "}",
              "```",
              "<div style=\"page-break-after: always;\"></div>",
              "**Recent form (14d vs 28d)**",
              "```{r form}",
              "knitr::kable(params$form, digits = 2)",
              "```",
              "**Stability (CV%, TE)**",
              "```{r stab}",
              "knitr::kable(params$stab, digits = 2)",
              "```"
            ), rmd)

            shiny::incProgress(0.75, detail = "Rendering HTML")
            rmarkdown::render(
              rmd,
              output_format = "html_document",
              output_file = html_out,
              quiet = TRUE,
              params = list(
                dd      = dd,
                form    = form_tab,
                stab    = stab_tab,
                athlete = input$rep_ath
              )
            )

            # 5 - Convert HTML to PDF
            shiny::incProgress(0.90, detail = "Converting to PDF")
            html_to_pdf(html_out, file)

            shiny::incProgress(1.00, detail = "Done")
          }
        )
      }
    )
  })
}
