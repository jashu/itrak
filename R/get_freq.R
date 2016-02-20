#' Calculate Sampling Frequency
#'
#' If you are unsure about the sampling frequency of your time series,
#' \code{get_freq} can verify this, provided your data contains a time stamp for
#' each measurement.
#'
#' @param time Numeric vector of time stamps.
#'
#' @param units String indicating units of time (either \code{"ms"} or
#' \code{"s"}). The default is \code{"ms"}.
#'
#' @return Sampling frequency in Hz.
#'
#' @export

get_freq <- function(time, units = "ms"){
  if(! units %in% c("s", "ms")) stop ("Units must be either \"s\" or \"ms\"")
  if(units == "ms"){
    time = time / 1000
  }
  n <- length(time)
  if(n > 100) n <- 100
  round(1 / mean(diff(time[1:n]), na.rm = T))
}
