#' plot method for bayesTestResults object
#' @export
#' @importFrom ggplot2 scale_fill_gradient2 geom_text geom_tile ggplot aes
#' @importFrom dplyr pull
#' @param bayesTestTable output from \code{\link{runABbayesTest}}
#' @param xAxis unquoted column name containing the data to plot on the x axis of the tile plot
#' @param yAxis unquoted column name containing the data to plot on the y axis of the tile plot
#' @param fill unquoted column name containing the data to file the tile plot
plot.bayesTestResults <- function(bayesTestTable, xAxis = .data$topScorerSampleSize, yAxis = .data$lowScorerSampleSize, fill = .data$probability, ...) {

  xAxisLabels =
  bayesTestTable |>
    pull({{xAxis}}) |>
    unique()

  yAxisLabels =
    bayesTestTable |>
    pull({{yAxis}}) |>
    unique()

  bayesTestTable |>
    mutate({{xAxis}} := factor({{xAxis}}, levels = sort(xAxisLabels)),
           {{yAxis}} := factor({{yAxis}}, levels = sort(yAxisLabels))) |>
    ggplot(aes(x = {{ xAxis }}, y = {{ yAxis }}, fill = {{ fill }})) +
    geom_tile() +
    geom_text(aes(label = round({{ fill }}, 2)), color = "#000000", size = 10) +
    scale_fill_gradient2(
      low = "#F2F2F2",
      high = "#5A124A",
      limits = c(0, 1),
      midpoint = 0.65
    )
}

#' plot method for powerTestResults object
#' @export
#' @param bayesTestTable output from \code{\link{runFrequentistPower}}
plot.powerTestResults <- function(powerTestTable, xAxis = .data$topScorerSampleSize, yAxis = .data$lowScorerSampleSize, fill = .data$power, ...) {

  plot.bayesTestResults(powerTestTable, {{xAxis}}, {{yAxis}}, {{fill}}) +
    scale_fill_gradient2(
      low = "#F2F2F2",
      high = "#cc0c48",
      limits = c(0, 1),
      midpoint = 0.65
    )

}
