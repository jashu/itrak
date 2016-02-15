#' Baseline normalization
#'
#' \code{normalize} normalizes the time series to a specified baseline
#' reference period.
#'
#' \eqn{(ts-baseline)/baseline} where \eqn{ts} is the time series and
#' \eqn{baseline} is the mean of the values belonging to the baseline period.
#'
#' @section Warning:
#' Interpolation of missing values should be done prior to running this
#' function. Time series with any \code{NA}s will be silently rejected, and a
#' vector consisting entirely of \code{NA}s will be returned.
#'
#' @param ts A time series, passed as a vector of chronologically ordered
#' observations separated by equal intervals of time.
#'
#' @param  baseline A logical vector indicating which elements of the time
#' series correspond to the baseline period.
#'
#' @param lim Vector of limits specifying minimum and maximum relative change
#' from baseline that is plausible for your experiment. Default is
#' \code{c(-.5,.5)}, meaning time series containing more than a 50\% decrease or
#' 50\% increase from baseline will either be rejected or thresholded, according
#' to the \code{threshold} parameter.
#'
#' @param threshold Logical value indicating whether or not thresholding to the
#' range specified in \code{lim} should be performed. If set to \code{FALSE}
#' (the default), time series that exceed \code{lim} will be rejected and a
#' vector of all \code{NA}s will be returned. If set to \code{TRUE},
#' the lower and upper \code{lim} will be used to impose a floor and ceiling,
#' respectively, on the normalization.
#'
#' @return A baseline-normalized copy of the time series.
#'
#' @export


normalize <- function(ts, baseline, lim = c(-0.5, 0.5), threshold = FALSE){
  if(length(ts)==0) stop("Time series is empty.")
  if(any(is.na(ts))){
    ts[] <- NA_real_
    return(ts)
  }
  if(any(ts == 0)) stop(
    "Time series contains 0 values.
Either it has already been normalized, or it contains artifacts.
If the latter, use fix_artifacts before normalize.")
  baseline_mean = mean(ts[baseline])
  ts <- (ts - baseline_mean) / baseline_mean
  if(any(ts < lim[1]) || any(ts > lim[2])){
    if(threshold){
      ts[ts < lim[1]] <- lim[1]
      ts[ts > lim[2]] <- lim[2]
    } else {
      ts[] <- NA_real_
    }
  }
  return(ts)
}
