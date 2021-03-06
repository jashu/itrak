#' Clip Trials to Standard Length
#'
#' Clips all trials to identical start and stop times, and issues a warning
#' if there are any missing samples.
#'
#' Due to a variety of factors, trials in an eye-tracking experiment end up
#' being somewhat variable in length. \code{clip_trials} is useful for trimming
#' multiple trials down to identical lengths, issuing a warning if your data
#' contain trials that are shorter than the standard length you have specified.
#'
#' Note that unlike most of the other functions in this package,
#' \code{clip_trials} operates on an entire data frame (not individual vectors).
#' The data frame must contain a column specifying trial number and a column
#' specifying time. The names of these columns should be passed, without quotes,
#' to the \code{trial} and \code{time} parameters, respectively.
#'
#' If you did not include a trial number label in your eye-tracking output, you
#' can use \code{\link{label_trials}} to construct one based on the sample
#' index. Importantly, you must first use the \code{\link{zero_trials}} function
#' to synchronize the \code{time} variable of each trial such that the 0 time
#' point of each trial corresponds to the onset of the experimental event. The
#' times in \code{start} and \code{stop} should be expressed in units relative
#' to this event, e.g., if the time scale is in milliseconds, arguments of
#' \code{start = -1000} and \code{stop = 6000} will standardize all trials to
#' begin 1 second before the event and end 6 seconds after the event.
#'
#' See the vignette on cleaning pupil data (\code{browseVignettes("itrak")}) for
#' an example of how to use this function.
#'
#' @seealso \code{\link{label_trials}}, \code{\link{zero_trials}}
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
#' @importFrom stats setNames
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
  data
}
