#' Plot Comparison of Original vs. Cleaned Time Series
#'
#' \code{plot_comparison} helps to visualize the transformation of your original
#' time series into a subsequent, cleaner version.
#'
#' Use this plotting function to evaluate the performance of the
#' \code{\link{fix_artifacts}}, \code{\link{normalize}}, and/or
#' \code{\link{low_pass_filter}} functions on cleaning your time series. Note
#' that if your orignal and cleaned time series are at different scales (e.g.,
#' if you are comparing a pre-normed version to a post-normed version),
#' the original time series will be projected to the scale of
#' the cleaned time series.
#'
#' @param data Data frame containing both time series.
#'
#' @param time Name of variable that gives time units.
#'
#' @param orig Name of variable containing the original (uncleaned) time series
#' of measures.
#'
#' @param clean Name of variable containing the cleaned time series of
#' measures.
#'
#' @param trial If the data contains multiple time series, name of the variable
#'  that identifies each time series. Will be plotted using \code{facet_wrap}.
#'
#' @export

plot_comparison <- function(data, time, orig, clean, trial = NULL){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time <- eval(arguments$time,data)
    orig <- eval(arguments$orig,data)
    clean <- eval(arguments$clean,data)
    trial <- eval(arguments$trial, data)
  }
  if(is.null(trial)){
    trial <- rep(1, length(time))
  }
  # scale original ts to match clean ts
  scale_ts <- function(orig, clean){
    start <- floor(length(orig) * 0.05)
    mean_diff <- mean(orig[1:start], na.rm = T) -
      mean(clean[1:start], na.rm = T)
    sd_orig <- sd(orig, na.rm = T)
    sd_clean <- sd(clean, na.rm = T)
    (orig - mean_diff) / (sd_orig / sd_clean)
  }
  data <- data_frame(time = time,
                     orig = orig,
                     clean = clean,
                     trial = trial)
  med_orig <- median(orig, na.rm = T)
  med_clean <- median(clean, na.rm = T)
  if(!dplyr::between(med_orig/med_clean, .5, 1.5)){
    data <- dplyr::group_by(data, trial)
    data <- dplyr::mutate(data, orig = scale_ts(orig, clean))
    data <- dplyr::ungroup(data)
  }
  ggplot(data) +
    geom_line(aes(time, orig), color = "red") +
    geom_line(aes(time, clean), color = "green") +
    facet_wrap(~trial) +
    scale_y_continuous(limits = range(clean, na.rm = T)) +
    ylab("") +
    theme_bw()
}
