#' Plot Comparison of Time Series
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
#' @param pre Name of variable containing the pre-processed time series.
#'
#' @param post Name of variable containing the post-processed time series.
#'
#' @param trial If the data contains multiple time series, name of the variable
#'  that identifies each time series. Will be plotted using \code{facet_wrap}.
#'
#' @importFrom stats median sd
#' @import ggplot2
#' @export

plot_comparison <- function(data, time, pre, post, trial = NULL){
  arguments <- as.list(match.call())
  if(!is.null(data)){
    time <- eval(arguments$time,data)
    pre <- eval(arguments$pre,data)
    post <- eval(arguments$post,data)
    trial <- eval(arguments$trial, data)
  }
  if(all(is.na(pre)) && all(is.na(post)))
    stop("There are no data to plot.")
  if(is.null(trial)){
    trial <- rep(1, length(time))
  }
  # scale old ts to match new ts
  scale_ts <- function(pre, post){
    start <- floor(length(pre) * 0.05)
    mean_diff <- mean(pre[1:start], na.rm = T) -
      mean(post[1:start], na.rm = T)
    sd_pre <- sd(pre, na.rm = T)
    sd_post <- sd(post, na.rm = T)
    (pre - mean_diff) / (sd_pre / sd_post)
  }
  data <- tibble::data_frame(time = time,
                             pre = pre,
                             post = post,
                             trial = trial)
  med_pre <- median(pre, na.rm = T)
  med_post <- median(post, na.rm = T)
  if(!is.na(med_pre) && !is.na(med_post) &&
     !dplyr::between(med_pre/med_post, .5, 1.5)){
    data <- dplyr::group_by(data, trial)
    data <- dplyr::mutate(data, pre = scale_ts(pre, post))
    data <- dplyr::ungroup(data)
  }
  data$pre[is.na(data$pre)] <- -Inf
  data$post[is.na(data$post)] <- -Inf
  ggplot(data) +
    geom_line(aes(time, pre), color = "red") +
    geom_line(aes(time, post), color = "green") +
    facet_wrap(~trial) +
    coord_cartesian(ylim = range(post, na.rm = T)) +
    ylab("") +
    theme_bw()
}
