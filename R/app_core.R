#' Run the VALD ForceDecks Performance Dashboard
#'
#' Launches an interactive 'shiny' application for exploring your ForceDecks data.
#'
#' @param data Optional ForceDecks data.frame. If NULL, the app will prompt for CSV files.
#' @param tabs Character vector of tabs to show.
#' @param launch.browser Logical. Launch app in a browser window.
#'
#' @return No return value, called for side effects (launches the 'shiny' application).
#'
#' @examples
#' if (interactive()) {
#'   forcedecks_performance_dashboard()
#' }
#'
#' @export
forcedecks_performance_dashboard <- function(
    data = NULL,
    launch.browser = TRUE,
    tabs = c("metric_explorer", "radar", "longit", "quadrant", "player_report")) {
    app <- shiny::shinyApp(
        ui = app_ui(enabled_tabs = tabs),
        server = function(input, output, session) {
            app_server(input, output, session, data = data)
        }
    )

    shiny::runApp(app, launch.browser = launch.browser)
}

#' @keywords internal
app_ui <- function(
    enabled_tabs = c("metric_explorer", "radar", "longit", "quadrant", "player_report")) {
    navs <- list()

    if ("metric_explorer" %in% enabled_tabs) {
        navs <- c(
            navs,
            list(
                bslib::nav_panel(
                    "Metric Explorer",
                    mod_metric_explorer_ui("metric_explorer")
                )
            )
        )
    }

    if ("radar" %in% enabled_tabs) {
        navs <- c(
            navs,
            list(
                bslib::nav_panel(
                    "Radar Chart",
                    mod_radar_ui("radar")
                )
            )
        )
    }

    if ("longit" %in% enabled_tabs) {
        navs <- c(
            navs,
            list(
                bslib::nav_panel(
                    "Compare",
                    mod_longit_athlete_comparison_ui("longit")
                )
            )
        )
    }

    if ("quadrant" %in% enabled_tabs) {
        navs <- c(
            navs,
            list(
                bslib::nav_panel(
                    "Quadrant",
                    mod_quadrant_ui("quadrant")
                )
            )
        )
    }

    if ("player_report" %in% enabled_tabs) {
        navs <- c(
            navs,
            list(
                bslib::nav_panel(
                    "Player Report",
                    mod_player_report_ui("player_report")
                )
            )
        )
    }

    bslib::page_fluid(
        theme = bslib::bs_theme(
            bootswatch = "flatly",
            base_font  = bslib::font_google("Roboto")
        ),
        shiny::tags$head(
            shiny::tags$style(APP_CSS),
        ),

        # Header
        shiny::div(
            class = "app-header",
            shiny::div(
                shiny::div(
                    class = "power-title",
                    "valdrViz | ForceDecks Performance Dashboard"
                ),
                shiny::div(
                    class = "app-subtitle",
                    "Analyse, compare and report on your ForceDecks testing data."
                )
            )
        ),

        # Body
        bslib::layout_sidebar(
            sidebar = bslib::sidebar(
                class = "card-like data-import-card",

                # Data import block
                shiny::h4("Data import", class = "data-import-section-title"),
                shiny::radioButtons(
                    "mode", "Source",
                    choices = c("Folder" = "folder", "Upload" = "upload"),
                    inline = TRUE
                ),

                # Folder mode
                shiny::conditionalPanel(
                    "input.mode=='folder'",
                    shiny::div(
                        class = "data-import-fields",
                        shiny::textInput("base_dir", "Input your folder path below:", ""),
                        shiny::actionButton("load_folder", "Load CSVs", class = "btn btn-primary")
                    )
                ),

                # Upload mode
                shiny::conditionalPanel(
                    "input.mode=='upload'",
                    shiny::div(
                        class = "data-import-fields",
                        shiny::fileInput(
                            "csv_files", "ForceDecks CSVs",
                            multiple = TRUE,
                            accept = c(".csv"),
                            buttonLabel = "Browse"
                        )
                    )
                ),
                shiny::hr(),

                # Filters block
                shiny::h4("Filters", class = "data-import-section-title"),
                shiny::selectInput("testtype", "Test Type", choices = NULL),
                shiny::dateRangeInput("date_rng", "Date Range", width = "100%"),
                shiny::selectizeInput(
                    "athletes", "Athletes",
                    choices = NULL,
                    multiple = TRUE,
                    options = list(placeholder = "Select athletes"),
                    width = "100%"
                )
            ),

            # Main content
            bslib::card(
                bslib::navset_card_tab(!!!navs)
            )
        )
    )
}
