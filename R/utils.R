# R/utils.R
# Internal helpers for valdrViz / jump monitoring app
#' Required ForceDecks columns
#'
#' Vector of column names expected in ForceDecks CSV exports.
#' @keywords internal
REQ <- c(
    "athleteId", "givenName", "familyName", "recordedUTC",
    "testType", "trialId", "weight", "resultName", "value", "resultUnitName"
)

# General utilities

`%||%` <- function(a, b) {
    if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
}

# Simple aliases so modules can call validate()/need() without namespacing
validate <- shiny::validate
need <- shiny::need

# ForceDecks I/O helpers

#' Read a single ForceDecks CSV
#'
#' Safely reads a CSV file and checks that all required columns are present.
#' Returns `NULL` on failure.
#'
#' @param path Path to a CSV file.
#'
#' @keywords internal
read_fd <- function(path) {
    df <- tryCatch(
        readr::read_csv(path, show_col_types = FALSE),
        error = function(e) NULL
    )

    if (is.null(df) || !all(REQ %in% names(df))) {
        return(NULL)
    }

    df
}

#' Standardise ForceDecks data
#'
#' Adds `Date` and `Athlete` columns and removes duplicate
#' `trialId`/`resultName` rows.
#'
#' @param df A data.frame read from one or more ForceDecks CSVs.
#'
#' @keywords internal
fd_standardise <- function(df) {
    df %>%
        dplyr::mutate(
            recordedUTC = lubridate::ymd_hms(.data$recordedUTC, quiet = TRUE),
            Date        = as.Date(.data$recordedUTC),
            Athlete     = paste(.data$givenName, .data$familyName)
        ) %>%
        dplyr::distinct(.data$trialId, .data$resultName, .keep_all = TRUE)
}

#' Read all valid ForceDecks CSVs from a folder
#'
#' Recursively scans a folder for `.csv` files, keeps only those that
#' contain the required ForceDecks columns, and returns a standardised
#' combined data.frame. Returns `NULL` if nothing valid is found.
#'
#' @param folder Folder path.
#'
#' @keywords internal
fd_read_folder <- function(folder) {
    files <- list.files(folder, "\\.csv$", full.names = TRUE, recursive = TRUE)
    if (!length(files)) {
        return(NULL)
    }

    lst <- lapply(files, read_fd)
    lst <- lst[!vapply(lst, is.null, logical(1))]

    if (!length(lst)) {
        return(NULL)
    }

    dplyr::bind_rows(lst) %>% fd_standardise()
}

# Plotting helpers

#' Palette helper for metric plots
#'
#' Returns a qualitative palette for up to ~12 metrics, then falls back
#' to `hcl.colors()` for larger sets.
#'
#' @param n Number of colours required.
#'
#' @keywords internal
metric_pal <- function(n) {
    if (n <= 12) {
        RColorBrewer::brewer.pal(max(3, n), "Set3")[seq_len(n)]
    } else {
        hcl.colors(n, "Dynamic")
    }
}

# HTML -> PDF helper

#' Convert a HTML file to PDF
#'
#' Tries `pagedown::chrome_print()` if Chrome is available,
#' otherwise falls back to `webshot2::webshot()`.
#'
#' @param html_file Path to source HTML file.
#' @param out_pdf   Path to output PDF.
#'
#' @keywords internal
html_to_pdf <- function(html_file, out_pdf) {
    ok <- TRUE

    has_chrome <- !is.na(
        tryCatch(pagedown::find_chrome(), error = function(e) NA)
    )

    if (has_chrome) {
        tryCatch(
            pagedown::chrome_print(
                input   = html_file,
                output  = out_pdf,
                verbose = 0,
                # key bit: ask Chrome to print in landscape
                options = list(landscape = TRUE)
            ),
            error = function(e) ok <<- FALSE
        )
    } else {
        ok <- FALSE
    }

    if (!ok) {
        # Fallback via webshot2, also landscape-ish (wider than tall)
        tryCatch(
            webshot2::webshot(
                html_file, out_pdf,
                vwidth = 1800, # wider
                vheight = 1300, # shorter
                delay = 2
            ),
            error = function(e) {
                stop("Failed to create PDF (Chrome/webshot missing).", call. = FALSE)
            }
        )
    }

    invisible(TRUE)
}

# Plotting helpers

#' Rotated x-axis theme for metric plots
#'
#' Used in Metric Explorer box/violin plots.
#'
#' @keywords internal
rot_theme <- ggplot2::theme(
    axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
    )
)

#' Shared ggplot theme
#' @keywords internal
viz_theme <- function(base_size = 14, base_family = "Roboto") {
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
        ggplot2::theme(
            plot.title   = ggplot2::element_text(face = "bold", hjust = 0),
            axis.title   = ggplot2::element_text(face = "bold"),
            strip.text   = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold")
        )
}

#' @keywords internal
style_viz_plotly <- function(p, font_family = "Roboto", font_size = 14) {
    p <- p |>
        plotly::layout(
            font   = list(family = font_family, size = font_size),
            legend = list(orientation = "h", x = 0, xanchor = "left", y = -0.25, yanchor = "top"),
            margin = list(l = 70, r = 30, t = 60, b = 150) # bigger bottom for rotated names
        ) |>
        plotly::config(displaylogo = FALSE)

    axis_names <- names(p$x$layout)
    for (ax in axis_names[grepl("^(xaxis|yaxis)", axis_names)]) {
        # ALWAYS allow plotly to allocate space for ticks/labels
        p$x$layout[[ax]]$automargin <- TRUE

        # keep your title-standoff logic if title exists
        if (!is.null(p$x$layout[[ax]]$title)) {
            if (is.character(p$x$layout[[ax]]$title)) {
                p$x$layout[[ax]]$title <- list(text = p$x$layout[[ax]]$title)
            } else if (is.null(p$x$layout[[ax]]$title$text) && !is.null(p$x$layout[[ax]]$title[[1]])) {
                p$x$layout[[ax]]$title$text <- as.character(p$x$layout[[ax]]$title[[1]])
            }

            if (grepl("^xaxis", ax)) p$x$layout[[ax]]$title$standoff <- 20
            if (grepl("^yaxis", ax)) p$x$layout[[ax]]$title$standoff <- 10
        }
    }

    # annotation fonts
    anns <- p$x$layout$annotations
    if (!is.null(anns) && length(anns)) {
        for (i in seq_along(anns)) {
            if (is.null(anns[[i]]$font)) anns[[i]]$font <- list()
            anns[[i]]$font$family <- font_family
            anns[[i]]$font$size <- font_size
        }
        p$x$layout$annotations <- anns
    }

    p
}



#' @keywords internal
as_viz_plotly <- function(g, tooltip = c("x", "y"), font_family = "Roboto", font_size = 14) {
    style_viz_plotly(
        plotly::ggplotly(g, tooltip = tooltip),
        font_family = font_family,
        font_size = font_size
    )
}

# Safe max func to return NA instead of errors if max fails
safe_max <- function(x) {
    if (is.null(x) || length(x) == 0) {
        return(NA_real_)
    }
    x <- x[is.finite(x) & !is.na(x)]
    if (length(x) == 0) NA_real_ else max(x)
}
