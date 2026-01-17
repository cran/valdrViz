#' @keywords internal
mod_metric_explorer_ui <- function(id) {
  ns <- NS(id)

  tagList(
    selectizeInput(
      ns("mx_metrics"), "Metrics",
      choices = NULL, multiple = TRUE,
      options = list(placeholder = "Pick metrics")
    ),
    tabsetPanel(
      tabPanel(
        "Boxplot",
        shinycssloaders::withSpinner(uiOutput(ns("mx_box_ui")))
      ),
      tabPanel(
        "Box+Jitter",
        shinycssloaders::withSpinner(uiOutput(ns("mx_boxjitter_ui")))
      ),
      tabPanel(
        "Violin",
        shinycssloaders::withSpinner(uiOutput(ns("mx_violin_ui")))
      ),
      tabPanel(
        "Violin+Box",
        shinycssloaders::withSpinner(uiOutput(ns("mx_violinbox_ui")))
      ),
      tabPanel(
        "Scatter",
        fluidRow(
          column(6, selectInput(ns("mx_sc_x"), "X", choices = NULL)),
          column(6, selectInput(ns("mx_sc_y"), "Y", choices = NULL))
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("mx_scatter"), height = "540px")
        ),
        uiOutput(ns("mx_scatter_info"))
      )
    )
  )
}

#' @keywords internal
mod_metric_explorer_server <- function(id, base_f) {
  moduleServer(id, function(input, output, session) {
    mx_facet_height <- reactive({
      n_mets <- length(input$mx_metrics %||% character(0))
      n_mets <- max(1, n_mets)

      ncol <- 2
      nrow <- max(1, ceiling(n_mets / ncol))

      # Adjusted for rotated names and plotly margins
      base <- 320
      per_row <- 260

      max(560, base + per_row * nrow)
    })


    # Initialise metric explorer inputs when data is available
    observeEvent(base_f(),
      {
        d <- base_f()
        mets <- sort(unique(d$resultName))
        if (!length(mets)) {
          return(NULL)
        }

        # Keep current metric selection if still valid
        cur_mx <- isolate(input$mx_metrics) %||% character(0)
        keep_mx <- intersect(cur_mx, mets)
        if (length(keep_mx) == 0) keep_mx <- head(mets, min(2, length(mets)))

        updateSelectizeInput(session, "mx_metrics", choices = mets, selected = keep_mx)

        # Keep scatter x/y if still valid
        cur_x <- isolate(input$mx_sc_x)
        cur_y <- isolate(input$mx_sc_y)

        x <- if (!is.null(cur_x) && cur_x %in% mets) cur_x else mets[1]
        y <- if (!is.null(cur_y) && cur_y %in% mets) cur_y else mets[min(2, length(mets))]

        # Prevent x == y
        if (!is.null(x) && !is.null(y) && x == y) {
          y <- mets[which(mets != x)[1]]
          if (is.na(y)) y <- x
        }

        updateSelectInput(session, "mx_sc_x", choices = mets, selected = x)
        updateSelectInput(session, "mx_sc_y", choices = mets, selected = y)
      },
      ignoreNULL = TRUE
    )

    output$mx_box_ui <- renderUI({
      plotly::plotlyOutput(session$ns("mx_box"), height = paste0(mx_facet_height(), "px"))
    })
    output$mx_boxjitter_ui <- renderUI({
      plotly::plotlyOutput(session$ns("mx_boxjitter"), height = paste0(mx_facet_height(), "px"))
    })
    output$mx_violin_ui <- renderUI({
      plotly::plotlyOutput(session$ns("mx_violin"), height = paste0(mx_facet_height(), "px"))
    })
    output$mx_violinbox_ui <- renderUI({
      plotly::plotlyOutput(session$ns("mx_violinbox"), height = paste0(mx_facet_height(), "px"))
    })

    output$mx_box <- plotly::renderPlotly({
      dd <- base_f() %>% dplyr::filter(resultName %in% input$mx_metrics)
      validate(need(nrow(dd) > 0, "No data"))

      g <- ggplot(dd, aes(Athlete, value, fill = Athlete)) +
        geom_boxplot(width = .7) +
        facet_wrap(~resultName, scales = "free_y", ncol = 2) +
        viz_theme() +
        rot_theme +
        theme(legend.position = "none")

      as_viz_plotly(g, tooltip = c("x", "y"))
    })

    output$mx_boxjitter <- plotly::renderPlotly({
      dd <- base_f() %>% dplyr::filter(resultName %in% input$mx_metrics)
      validate(need(nrow(dd) > 0, "No data"))

      g <- ggplot(dd, aes(Athlete, value, fill = Athlete)) +
        geom_boxplot(width = .7, outlier.shape = NA, alpha = .45) +
        geom_jitter(width = .2, alpha = .60, size = 1.7) +
        facet_wrap(~resultName, scales = "free_y", ncol = 2) +
        viz_theme() +
        rot_theme +
        theme(legend.position = "none")

      as_viz_plotly(g, tooltip = c("x", "y"))
    })

    output$mx_violin <- plotly::renderPlotly({
      dd <- base_f() %>% dplyr::filter(resultName %in% input$mx_metrics)
      validate(need(nrow(dd) > 0, "No data"))

      g <- ggplot(dd, aes(Athlete, value, fill = Athlete)) +
        geom_violin(width = .9, trim = FALSE, scale = "width", alpha = .45) +
        facet_wrap(~resultName, scales = "free_y", ncol = 2) +
        viz_theme() +
        rot_theme +
        theme(legend.position = "none")

      as_viz_plotly(g, tooltip = c("x", "y"))
    })

    output$mx_violinbox <- plotly::renderPlotly({
      dd <- base_f() %>% dplyr::filter(resultName %in% input$mx_metrics)
      validate(need(nrow(dd) > 0, "No data"))

      g <- ggplot(dd, aes(Athlete, value, fill = Athlete)) +
        geom_violin(width = .9, trim = FALSE, scale = "width", alpha = .35) +
        geom_boxplot(width = .22, outlier.size = .7) +
        facet_wrap(~resultName, scales = "free_y", ncol = 2) +
        viz_theme() +
        rot_theme +
        theme(legend.position = "none")

      as_viz_plotly(g, tooltip = c("x", "y"))
    })

    # Scatter
    output$mx_scatter <- plotly::renderPlotly({
      req(input$mx_sc_x, input$mx_sc_y, input$mx_sc_x != input$mx_sc_y)

      dd <- base_f() %>%
        dplyr::filter(resultName %in% c(input$mx_sc_x, input$mx_sc_y)) %>%
        dplyr::group_by(Athlete, resultName, Date) %>%
        dplyr::summarise(val = safe_max(value), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = resultName, values_from = val)

      validate(need(nrow(dd) > 4, "Not enough paired points"))

      x <- dd[[input$mx_sc_x]]
      y <- dd[[input$mx_sc_y]]
      r2 <- summary(lm(y ~ x))$r.squared

      g <- ggplot(dd, aes(.data[[input$mx_sc_x]], .data[[input$mx_sc_y]], color = Athlete)) +
        geom_point(size = 2, alpha = .85) +
        geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
        labs(
          title = sprintf("Scatter: %s vs %s", input$mx_sc_x, input$mx_sc_y),
          x = input$mx_sc_x, y = input$mx_sc_y
        ) +
        viz_theme()

      as_viz_plotly(g, tooltip = c("x", "y", "color"))
    })

    output$mx_scatter_info <- renderUI({
      HTML("<small>Paired by Athlete & Date (best-of-day). Linear fit across all.</small>")
    })
  })
}
