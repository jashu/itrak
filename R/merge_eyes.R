#' Merge Eyes
#'
#' Aggregate parallel time series from the left and right eyes into a single
#' time series of measurements.
#'
#' @param left Numeric vector containing the time series of measurements for the
#'  left eye.
#'
#' @param right Numeric vector containing the time series of measurements for
#' the right eye.
#'
#' @return Numeric vector. For binocular readings, the mean time series for
#' both eyes. For monocular readings, the time series for the eye that was
#' tracked.
#'
#' @export

merge_eyes <- function(left, right){
  suppressWarnings(left <- as.numeric(left))
  suppressWarnings(right <- as.numeric(right))
  eyes <- cbind(left, right)
  rowMeans(eyes, na.rm = TRUE)
}
