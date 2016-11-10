#' Zero Time Index to Trial Onset
#'
#' Centers the sample index or time stamp of your time series to a reference
#' marker, such that 0 always coincides with the onset of a trial.
#'
#' @param time Numeric vector of sample indicies or time stamps corresponding to
#' a single time series.
#'
#' @param marker Vector containing the indicator of trial \code{onset}.
#'
#' @param onset Value of \code{marker} that indicates when the trial begins.
#'
#' @return Numeric vector that zeros \code{time} to the trial onset. In the
#' event that no onset marker is found, a warning is issued and a vector of
#' \code{NA}s is returned.
#'
#' @export

zero_onset <- function(time, marker, onset){
  onset_time <- time[!is.na(marker) & marker == onset]
  if(length(onset_time) == 0){
    warning (paste(onset, "not found in onset marker"))
    return(rep(NA, length(time)))
  }
  if(length(onset_time) > 1)
    stop ("More than one instance of onset marker found.")
  time - onset_time
}
