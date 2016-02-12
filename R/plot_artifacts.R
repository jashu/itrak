#' Plot artifacts
#'
#' \code{plot_artifacts} returns a plot of a single time series with
#' artifact intervals marked in red with vertical dotted lines.
#'
#' Use this plotting function to evaluate the performance of the
#' \code{\link{get_artifacts}} function on individual time series and the
#' impact of changes to its threshold settings. If many artifacts are
#' going undetected, try decreasing the \code{max_velocity} setting. If there
#' are too many false positives, try increaasing it.
#'
#' @param data Data frame containing time series.
#'
#' @param time Name of variable that gives time units. Will be plotted on the x-
#' axis.
#'
#' @param pupil Name of variable containing the time series of pupil measures.
#' Will be plotted on the y-axis.
#'
#' @param artifacts Name of variable containing logical vector that indexes
#' which observations in the \code{pupil} time series are artifacts.
#' See \code{\link{get_artifacts}}.
#'
#' @param trial If the data contains multiple time series, name of the variable
#'  that identifies each time series. Will be plotted using \code{facet_wrap}.
#'
#' @export

plot_artifacts <- function(data, time, pupil, artifacts, trial = NULL){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time = eval(arguments$time,data)
    pupil = eval(arguments$pupil,data)
    artifacts = eval(arguments$artifacts,data)
    trial = eval(arguments$trial, data)
  }
  if(is.null(trial)){
    trial <- rep(1, length(time))
  }
  pupil_data <- data_frame(time = time,
                           pupil = pupil,
                           artifacts = artifacts,
                           trial = trial)
  artifacts <- pupil_data %>% filter(artifacts)
  mean_pupil <- mean(pupil_data$pupil, na.rm = T)
  limits <- c(.5*mean_pupil, 1.5*mean_pupil)
  ggplot() +
    geom_line(aes(time, pupil), pupil_data) +
    geom_vline(aes(xintercept = time), artifacts,
               color = "red", linetype = "dotted") +
    scale_y_continuous(limits = limits) +
    facet_wrap(~trial) +
    theme_bw()
}
