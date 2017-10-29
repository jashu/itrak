#' Identify Outliers
#'
#' \code{is_outlier} flags which observations fall outside a valid range of
#' values based on limits set by the user. Limits can be set in absolute
#' terms (the units of measurement), median absolute deviations, standard
#' deviations, or any combination of the above.
#'
#' If more than one type of limit is specified, \code{is_outlier} will first
#' apply the absolute limits if given (so that values that are outright
#' impossible do not factor into the determination of the deviation statistics),
#' followed by the median-absolute-deviation (MAD) test and/or the
#' standard-deviation test.
#'
#' The \code{mad_lim} argument is evaluated using the \emph{double MAD}, which
#' provides for robust identification of outliers even when the underlying
#' distribution is non-normal and/or asymmetric. See
#' \href{http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers}{Peter Rosenmai's blog post}
#' for more information.
#'
#' @param measure A numeric vector.
#'
#' @param abs_lim Two-item numeric vector \code{c(lower, upper)}
#' specifying the \emph{absolute} lower and upper limits, respectively, of the
#' range of valid values in terms of \emph{measurement units}. This range should
#' be set such that values that fall outside of it are considered implausible or
#' impossible.
#'
#' @param mad_lim Numeric value specifying the range of valid values in terms of
#' \emph{median absolute deviations} from the median.
#'
#' @param sd_lim Numeric value specifying the range of valid values in terms of
#'  \emph{standard deviations} from the mean.
#'
#' @return A logical vector indicating \code{TRUE} if an observation is an
#'  outlier.
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
#' # Check for trials that are more than 3 standard deviations from the mean
#' is_outlier(rt, sd_lim = 3)
#'
#' # Check for trials that are more than 3 median absolute deviations from the
#' # median
#' is_outlier(rt, mad_lim = 3)
#'
#' # Check for trials that are less than 250 ms, more than 2500 ms, or more than
#' # 2.5 MADs from the median:
#' is_outlier(rt, abs_lim = c(250, 2500), mad_lim = 2.5)
#'
#' @references
#' Rosenmai P. 2013.
#' \href{http://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers}{Using the median absolute deviation to find outliers.}
#'
#' @export

is_outlier <- function(measure, abs_lim = NULL, mad_lim = NULL, sd_lim = NULL){
  if(is.null(abs_lim) && is.null(mad_lim) && is.null(sd_lim))
    stop("You must specify at least one `_lim` argument.")
  if(!is.numeric(measure))
    stop("The measure is not stored as a numeric type.")
  if(!is.null(abs_lim) && (length(abs_lim) != 2 || !is.numeric(abs_lim)))
    stop("The absolute limit must be given as a two-element vector.")
  if(!is.null(mad_lim) && (length(mad_lim) != 1 || !is.numeric(mad_lim)))
    stop("The MAD limit must be a single number.")
  if(!is.null(sd_lim) && (length(sd_lim) != 1 || !is.numeric(sd_lim)))
    stop("The stand. dev. limit must be a single number.")
  outlier <- is.na(measure)
  if(!is.null(abs_lim)){
    outlier[!outlier] <- !dplyr::between(measure[!outlier],
                                         abs_lim[1], abs_lim[2])
    if(all(outlier)) return(outlier)
  }
  if(!is.null(mad_lim)){
    outlier[!outlier] <- DoubleMADsFromMedian(measure[!outlier]) > mad_lim
  }
  if(!is.null(sd_lim)){
    outlier[!outlier] <- abs(scale(measure[!outlier])) > sd_lim
  }
  outlier[is.na(measure)] <- NA
  outlier
}

#' @importFrom stats median

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

#' @importFrom stats median

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
