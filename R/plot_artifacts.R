#' Plot Artifacts
#'
#' \code{plot_artifacts} returns a plot of time series with artifact intervals
#' marked in red with dotted lines. Extreme values are marked with blue dotted
#' lines.
#'
#' Use this plotting function to evaluate the performance of the
#' \code{\link{get_artifacts}} and optionally \code{\link{get_oor}} functions on
#' one or more time series. If many artifacts are going undetected, try
#' decreasing the \code{max_velocity} setting. If there are too many false
#' positives, try increaasing it.
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
#' @param oor Optional name of variable containing logical vector that indexes
#' which observations in the \code{measure} time series are out of range.
#' See \code{\link{get_oor}}.
#'
#' @param trial If the data contains multiple time series, name of the variable
#'  that identifies each time series. Will be plotted using \code{facet_wrap}.
#'
#' @export

plot_artifacts <- function(data, time, measure, artifacts, oor = NULL,
                           trial = NULL){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time <- eval(arguments$time,data)
    measure <- eval(arguments$measure,data)
    artifacts <- eval(arguments$artifacts, data)
    oor <-eval(arguments$oor, data)
    trial <- eval(arguments$trial, data)
  }
  if(is.null(oor)){
    oor <- rep(FALSE, length(time))
  }
  if(is.null(trial)){
    trial <- rep(1, length(time))
  }
  IQR <- quantile(measure, c(.25, .75), na.rm = TRUE)
  ymin <- IQR[1] - (1.5 * (IQR[2] - IQR[1]))
  ymax <- IQR[2] + (1.5 * (IQR[2] - IQR[1]))
  measure[is.na(measure)] <- -Inf
  measure_data <- tibble::data_frame(time = time,
                                     measure = measure,
                                     artifacts = artifacts,
                                     oor = oor,
                                     trial = trial)
  artifacts <- dplyr::filter(measure_data, artifacts)
  oor <- dplyr::filter(measure_data, oor)
  ggplot() + facet_wrap(~trial) +
    geom_line(aes(time, measure), measure_data) +
    geom_vline(aes(xintercept = time), artifacts,
               color = "red", linetype = "dotted") +
    geom_vline(aes(xintercept = time), oor,
               color = "blue", linetype = "dotted") +
    coord_cartesian(ylim = c(ymin, ymax)) +
    ylab("") +
    theme_bw()
}
