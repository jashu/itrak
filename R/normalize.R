#' Normalize Time Series
#'
#' \code{normalize} normalizes the time series to a specified baseline
#' reference period using \eqn{(ts-baseline)/baseline} where \eqn{ts} is the
#' time series and \eqn{baseline} is the mean of the values belonging to the
#' baseline period.
#'
#' @section Warning:
#' Interpolation of missing values should be done prior to running this
#' function. Time series with any \code{NA}s will be silently rejected by
#' returning a vector consisting entirely of \code{NA}s.
#'
#' @seealso  \code{\link{artifacts}}, \code{\link{low_pass_filter}}
#'
#' @param ts A time series, passed as a vector of chronologically ordered
#' observations separated by equal intervals of time.
#'
#' @param baseline A logical vector indicating which elements of the time
#' series correspond to the baseline period.
#'
#' @return A baseline-normalized copy of the time series converted to units of
#' relative change (difference from baseline as a fraction of baseline).
#'
#' @export


normalize <- function(ts, baseline){
  if(length(ts)==0) stop("Time series is empty.")
  if(any(is.na(ts))){
    ts[] <- NA_real_
    return(ts)
  }
  if(any(ts <= 0)) stop(
"Time series contains negative values. Perhaps it has already been normalized?")
  baseline_mean = mean(ts[baseline])
  (ts - baseline_mean) / baseline_mean
}
