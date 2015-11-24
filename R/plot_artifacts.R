#' Plot artifacts
#'
#' \code{plot_artifacts} returns a plot of a single time series with
#' artifact intervals marked in red with vertical dotted lines.
#'
#' Use this plotting function to evaluate the performance of the
#' \code{\link{get_artifacts}} function on individual time series and the
#' impact of changes to its threshold settings. If many artifacts are
#' going undetected, try decreasing the \code{change_cutoff} and/or the
#' \code{prepost_cutoff} values. If there are too many false positives, try
#' increaasing one or both of these settings.
#'
#' @param time Numeric vector corresponding to time. Will be plotted on the x-
#' axis. Must be the same length as the \code{pupil} and \code{artifact}
#' vectors.
#'
#' @param pupil Numeric vector containing the time series of pupil measurements.
#' Will be plotted on the y-axis. Must be the same length as the \code{time}
#' and \code{artifact} vectors.
#'
#' @param artifacts Logical vector that indexes which observations in the
#' \code{pupil} time series are artifacts. See \code{\link{get_artifacts}}.
#'
#' @param data Data frame to use (optional).
#'
#' @param ... optional arguments to \code{\link[ggplot2]{qplot}},
#' e.g., "main", "ylim", etc.
#'
#' @export

plot_artifacts <- function(time, pupil, artifacts, data = NULL, ...){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time = eval(arguments$time,data)
    pupil = eval(arguments$pupil,data)
    artifacts = eval(arguments$artifacts,data)
  }
  ggplot2::qplot(time, pupil, geom = "line", ...) +
    ggplot2::geom_vline(xintercept = time[artifacts], color = "red",
                        linetype = "dotted") + theme_bw()
}
