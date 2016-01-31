#' Identify Outliers
#'
#' \code{is_outlier} flags which observations fall outside a valid range of
#' values based on limits set by the user. Limits can be set in absolute
#' terms (the units of measurement), standard deviations, or both.
#'
#' If both absolute and standard-deviation limits are specified, then
#' \code{is_outlier} will first apply the absolute-deviation test, followed
#' by the standard-deviation test for only those observations that pass the
#' absolute-deviation test (so that values that are outright impossible do not
#' factor into the determination of standard deviation).
#'
#' \code{is_outlier} uses a robust version of median absolute deviation to
#' estimate the population standard deviation, which remains valid even for
#' non-normal and/or asymmetric distributions. See \href{
#' http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers}{
#' Peter Rosenmai's blog post} for more information.
#'
#' @param measure A numeric vector.
#'
#' @param abs_lim Two-item numeric vector \code{c(lower, upper)}
#' specifying the \emph{absolute} lower and upper limits, respectively, of the
#' range of valid values in terms of \emph{measurement units}. This range should
#' be set such that values that fall outside of it are considered implausible or
#' impossible.
#'
#' @param sd_lim Numeric value specifying the \emph{relative} range of valid
#' values in terms of \emph{standard deviation}.
#'
#' @return A logical vector indicating \code{TRUE} if the trial is outside the
#' given range of valid values.
#'
#' @examples
#' # Create example time series of 10 reaction times in ms with two trials that
#' # fall outside the bounds of validity:
#' set.seed(7)
#' rt <- c(0, 10000, rnorm(8, mean = 1000, sd = 250))
#' rt
#' # Check for trials that are less than 250 ms with no upper bound:
#' is_outlier(rt, abs_lim = c(250, Inf))
#'
#' # Check for trials that are more than 2 standard deviations from the mean
#' is_outlier(rt, sd_lim = 2)
#'
#' # Check for trials that are less than 250 ms, more than 2500 ms, or more than
#' # 2 standard deviations from the mean:
#' is_outlier(rt, abs_lim = c(250, 2500), sd_lim = 2)
#'
#' @references Rosemani, P. 2013. \href{
#' http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers}{
#' Using the median absolute deviation to find outliers.}
#'
#' @export

is_outlier <- function(measure, abs_lim = NULL, sd_lim = NULL){
  if(is.null(abs_lim) && is.null(sd_lim))
    stop("You must specify either `abs_lim` or `sd_lim`.")
  if(!is.numeric(measure))
    stop("The measure is not stored as a numeric type.")
  if(!is.null(abs_lim) && (length(abs_lim) != 2 || !is.numeric(abs_lim)))
    stop("The absolute limit must be given as a two-element vector.")
  if(!is.null(sd_lim) && (length(sd_lim) != 1 || !is.numeric(sd_lim)))
    stop("The stand. dev. limit must be a single number.")
  outlier <- rep(FALSE, length(measure))
  if(!is.null(abs_lim)){
    outlier <- !dplyr::between(measure, abs_lim[1], abs_lim[2])
  }
  if(!is.null(sd_lim)){
    outlier[!outlier] <- DoubleMADsFromMedian(measure[!outlier]) > sd_lim
  }
  outlier
}

DoubleMAD <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x         <- x[!is.na(x)]
  m         <- median(x)
  abs.dev   <- abs(x - m)
  left.mad  <- median(abs.dev[x<=m])
  right.mad <- median(abs.dev[x>=m])
  if (left.mad == 0 || right.mad == 0){
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c(  "na", "warn and na")){
      if (left.mad  == 0) left.mad  <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}

DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  two.sided.mad <- DoubleMAD(x, zero.mad.action)
  m <- median(x, na.rm=TRUE)
  x.mad <- rep(two.sided.mad[1], length(x))
  x.mad[x > m] <- two.sided.mad[2]
  mad.distance <- abs(x - m) / x.mad
  mad.distance[x==m] <- 0
  return(mad.distance)
}
