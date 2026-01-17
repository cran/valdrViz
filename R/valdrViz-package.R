#' valdrViz: VALD ForceDecks visualisations
#'
#' Tools to explore and report ForceDecks testing data, including metric exploration,
#' radar charts, longitudinal comparisons, quadrants and player reports.
#'
#' @keywords internal
"_PACKAGE"

#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import tibble
#' @import zoo
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom rlang .data
#' @importFrom utils head
#' @importFrom stats lm sd
#' @importFrom stats setNames
#' @importFrom grDevices hcl.colors
NULL

#' Forward pipe operator
#'
#' See 'magrittr::%>%' for details.
#'
#' @param lhs,rhs Objects to pipe.
#' @return The result of applying the right-hand side expression to the left-hand side value.
#'
#' @name %>%
#' @rdname pipe
#' @importFrom magrittr %>%
#' @export
`%>%` <- magrittr::`%>%`

utils::globalVariables(c(
    "Athlete", "resultName", "Date", "value", "val", "v", "mean14", "mean28",
    "delta_perc", "sd", "CV", "TE", "days", "roll", "Series", "slope",
    "Slope (per day)", "p-value", ".hover"
))
