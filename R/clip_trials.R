#' Clip Trials to Standard Length
#'
#' Clips all trials to the specified start and stop times, and issues a warning
#' if there are any missing samples.
#'
#' @param data Data frame containing all of your time series data and metadata.
#'
#' @param trial Name of the variable containing the trial label.
#'
#' @param time Name of the variable containing the time stamp.
#'
#' @param start Value for start time. Times earlier than this will be discarded.
#'
#' @param stop Value for stop time. Times later than this will be discarded.
#'
#' @return Copy of data frame reduced to the range of times given by
#' \code{start} and \code{stop}.
#'
#' @export
#'
clip_trials <- function(data, trial, time, start, stop){
  arguments <- as.list(match.call())
  arg1 <- lazyeval::lazy(max(time) - min(time))
  arg2 <- lazyeval::lazy(round(mean(diff(time))))
  dots <- list(arg1, arg2)
  trial <- lazyeval::lazy(trial)
  time <- eval(arguments$time, data)
  data <- data[dplyr::between(time, start, stop),]
  data_sum <- dplyr::group_by_(data, trial)
  data_sum <- dplyr::summarize_(data_sum,
                            .dots = setNames(dots, c("trial_len", "samp_len")))
  data_sum$missing <- (stop - start - data_sum$trial_len) / data_sum$samp_len
  total_missing <- sum(data_sum$missing)
  if(total_missing > 0) warning(paste("You are missing a total of",
                                      total_missing, "samples."))
  return(data)
}
