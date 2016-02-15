#' Artifact correction
#'
#' \code{fix_artifacts} Corrects artifacts using a sequence of linear
#' interpolation for internal artifacts (artifacts sandwiched between good data)
#' followed by matched lag-1 differences to overwrite external artifacts
#' (when the trial starts or ends with an artifact).
#'
#' @section Warning:
#' If the time series contains gaps that are too long for interpolation, the
#' function will silently return a time series consisting of all missing values
#' so that the user can easily identify which trials need to be dropped.
#'
#' @param ts A time series, passed as a numeric vector of chronologically
#' ordered, positively valued observations separated by equal intervals of time.
#'
#' @param samp_freq The sampling frequency in Hz.
#'
#' @param artifacts A logical vector of equal length to the time series that
#' provides logical indexing into which entries of the time series correspond
#' to artifacts.
#'
#' @param max_gap The maximum missing time interval in seconds to fill. The
#' default setting is 1 sec.
#'
#' @return A copy of the time series with artifacts and missing data replaced
#' by interpolated values.
#'
#' @seealso \code{\link{get_artifacts}}, \code{\link[zoo]{na.approx}}
#'
#' @export

fix_artifacts = function(ts, samp_freq, artifacts, max_gap = 1){
  # store the number of observations in the time series as 'n'
  n <- length(ts)
  # calculate the run-lengths of consecutive artifact vs. artifact-free periods
  # across the whole time series
  runs <- rle(artifacts)
  # extract from this the lengths of the artifact periods, i.e., the gaps in
  # signal
  gap <- runs$lengths[runs$values]
  # if any gaps exceed the max_gap threshold, set all values of the time series
  # to missing; else set only observations flagged as artifacts to missing
  if(any(gap > samp_freq * max_gap)) ts[] <- NA_real_
  ts[artifacts] <- NA_real_
  # if the entire time series is now missing (i.e., one or more artifact
  # periods was greater than the maximum allowable gap), do not proceed
  if(all(is.na(ts))) return(ts)
  # catch rare circumstance in which the entire time series consists of a
  # single run of valid data sandwiched between two runs of artifacts, which
  # will break the zoo package's na.approx function
  will_break_zoo <- length(runs$values) == 3 && all(runs$values == c(T, F, T))
  # linear interpolation for internal artifacts
  if(!will_break_zoo) ts <- zoo::na.approx(ts, na.rm = F)
  # if there is missing data at end of trial
  if(is.na(ts[n])){
    # h = the length of the final gap in the trial
    h = gap[length(gap)]
    # [n-2h : n-h] = the interval immediately preceding the final gap that is
    # equal in length to the final gap plus 1
    matched_interval = ts[(n-2*h):(n-h)]
    # if the length of the final gap is more than half the length of the entire
    # time series, or if there are any missing values in the matched interval,
    # cancel the interpolation.
    if(h > n/2 || any(is.na(matched_interval))){
      ts <- rep(NA_real_, n)
      return(ts)
      }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(matched_interval)
    # and repeat these difference over the missing interval
    for(i in 1:h){
      ts[n-h+i] <- ts[n-h+i-1] + lag1[i]
    }
  }
  # if there is missing data at start of trial
  if(is.na(ts[1])){
    # h = the length of the initial gap in the trial
    h = gap[1]
    # [h+1 : 2h+1] = the interval immediately following the first gap that is
    # equal in length to the first gap plus 1
    matched_interval = ts[(h+1):(2*h+1)]
    # if the length of the initial gap is more than half the length of the
    # entire time series, or if there are any missing values in the matched
    # interval, cancel the interpolation.
    if(h > n/2 || any(is.na(matched_interval))){
      ts <- rep(NA_real_, n)
      return(ts)
    }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(matched_interval)
    # and repeat these difference over the missing interval
    for(i in h:1){
      ts[i] <- ts[i+1] - lag1[i]
    }
  }
  return(ts)
}
