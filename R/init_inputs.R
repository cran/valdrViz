# R/init_inputs.R
# Helpers to initialise / reset inputs when data is loaded

#' @keywords internal
init_main_inputs <- function(session, d) {
    aths <- sort(unique(d$Athlete))

    # Test type
    updateSelectInput(
        session, "testtype",
        choices = sort(unique(d$testType)),
        selected = unique(d$testType)[1]
    )

    # Date range
    rng <- range(d$Date, na.rm = TRUE)
    updateDateRangeInput(
        session, "date_rng",
        start = rng[1],
        end = rng[2]
    )

    # Athletes
    updateSelectizeInput(
        session, "athletes",
        choices = aths,
        selected = aths,
        options = list(
            placeholder = "Select athletes"
        )
    )
}

# Metric Explorer

#' @keywords internal
init_metric_explorer_inputs <- function(session, mets) {
    # No defaults - this works for any testType
    updateSelectizeInput(
        session, "mx_metrics",
        choices = mets,
        selected = NULL,
        options = list(
            placeholder = "Select one or more metrics"
        )
    )

    updateSelectInput(
        session, "mx_sc_x",
        choices = mets,
        selected = NULL
    )

    updateSelectInput(
        session, "mx_sc_y",
        choices = mets,
        selected = NULL
    )
}

# Radar

init_radar_inputs <- function(session, mets, aths) {
    updateSelectizeInput(
        session, "radar_metrics",
        choices = mets,
        selected = NULL,
        options = list(
            placeholder = "Pick metrics for radar"
        )
    )

    updateSelectizeInput(
        session, "radar_athletes",
        choices = aths,
        selected = NULL,
        options = list(
            placeholder = "Select athletes"
        )
    )
}

# Longitudinal compare

#' @keywords internal
init_longit_inputs <- function(session, mets, aths) {
    updateSelectizeInput(
        session, "comp_athletes",
        choices = aths,
        selected = NULL,
        options = list(
            placeholder = "Choose athletes to compare"
        )
    )

    updateSelectizeInput(
        session, "comp_metrics",
        choices = mets,
        selected = NULL,
        options = list(
            placeholder = "Choose metrics to compare"
        )
    )
}

# Player report

#' @keywords internal
init_player_report_inputs <- function(session, mets, aths) {
    updateSelectInput(
        session, "rep_ath",
        choices = aths,
        selected = if (length(aths)) aths[1] else NULL
    )

    updateSelectizeInput(
        session, "rep_metrics",
        choices = mets,
        selected = NULL,
        options = list(
            placeholder = "Select metrics to include in report"
        )
    )
}
