#' Plot Artifacts
#'
#' \code{plot_artifacts} returns a plot of time series with
#' artifact intervals marked in red with vertical dotted lines.
#'
#' Use this plotting function to evaluate the performance of the
#' \code{\link{get_artifacts}} function on one or more time series and the
#' impact of changes to its threshold settings. If many artifacts are
#' going undetected, try decreasing the \code{max_velocity} setting. If there
#' are too many false positives, try increaasing it.
#'
#' Plotting is performed using \code{\link[ggplot2]{ggplot}} and you can modify
#' the aesthetics of the plot by appending layers using the \code{+} operator.
#'
#' @seealso  \code{\link{get_artifacts}}, \code{\link[ggplot2]{ggplot}}
#'
#' @param data Data frame containing time series.
#'
#' @param time Name of variable that gives time units. Will be plotted on the x-
#' axis.
#'
#' @param measure Name of variable containing the time series of measures.
#' Will be plotted on the y-axis.
#'
#' @param artifacts Name of variable containing logical vector that indexes
#' which observations in the \code{measure} time series are artifacts.
#' See \code{\link{get_artifacts}}.
#'
#' @param trial If the data contains multiple time series, name of the variable
#'  that identifies each time series. Will be plotted using \code{facet_wrap}.
#'
#' @export

plot_artifacts <- function(data, time, measure, artifacts, trial = NULL){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time = eval(arguments$time,data)
    measure = eval(arguments$measure,data)
    artifacts = eval(arguments$artifacts,data)
    trial = eval(arguments$trial, data)
  }
  if(is.null(trial)){
    trial <- rep(1, length(time))
  }
  IQR <- quantile(measure, c(.25, .75), na.rm = TRUE)
  ymin <- IQR[1] - (1.5 * (IQR[2] - IQR[1]))
  ymax <- IQR[2] + (1.5 * (IQR[2] - IQR[1]))
  measure_data <- data_frame(time = time,
                           measure = measure,
                           artifacts = artifacts,
                           trial = trial)
  artifacts <- measure_data %>% filter(artifacts)
  ggplot() +
    geom_line(aes(time, measure), measure_data) +
    geom_vline(aes(xintercept = time), artifacts,
               color = "red", linetype = "dotted") +
    scale_y_continuous(limits = c(ymin, ymax)) +
    facet_wrap(~trial) +
    ylab("") +
    theme_bw()
}
