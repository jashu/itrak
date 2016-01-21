#' Identify Out-of-Range Observations
#'
#' \code{out_of_range} flags which observations fall outside a valid range of
#' values based on limits set by the user. Limits can be set in absolute
#' terms (the units of measurement), standard deviations from the mean, or both.
#' If both types of limit are specified, then \code{out_of_range} will
#' first apply the absolute-deviation test, followed by the standard-deviation
#' test for only those observations that pass the absolute-deviation test (so
#' that values that are outright impossible do not factor into the calculation
#' of the mean and standard deviation).
#'
#' @param measure A vector of chronologically ordered observations of numeric
#'  type.
#'
#' @param abs_limit Optional two-item numeric vector \code{c(lower, upper)}
#' specifying the \emph{absolute} lower and upper limits, respectively, of the
#' range of valid values in terms of \emph{measurement units}. This range should
#' be set such that values that fall outside of it are considered implausible or
#' impossible.
#'
#' @param sd_limit Numeric value specifying the \emph{relative} range of valid
#' values in terms of \emph{standard deviation}. This should be set such that
#' values that fall outside of +/- \code{sd_limit} from the mean are considered
#' outliers. Defaults to 3 SD if not specified.
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
#' out_of_range(rt, abs_limit = c(250, Inf))
#'
#' # Check for trials that are more than 2 standard deviations from the mean
#' out_of_range(rt, sd_limit = 2)
#'
#' # Check for trials that are less than 250 ms, more than 2500 ms, or more than
#' # 2 standard deviations from the mean:
#' out_of_range(rt, abs_limit = c(250, 2500), sd_limit = 2)
#'
#' @export

out_of_range <- function(measure, abs_limit = NULL, sd_limit = NULL){
  if(is.null(abs_limit) && is.null(sd_limit))
    stop("You must specify either `abs_limit` or `sd_limit`.")
  if(!is.numeric(measure))
    stop("The measure is not stored as a numeric type.")
  if(!is.null(abs_limit) && (length(abs_limit) != 2 || !is.numeric(abs_limit)))
    stop("The absolute limit must be given as a two-element vector.")
  if(!is.null(sd_limit) && (length(sd_limit) != 1 || !is.numeric(sd_limit)))
    stop("The stand. dev. limit must be a single number.")
  oor <- rep(FALSE, length(measure))
  if(!is.null(abs_limit)){
    oor <- !dplyr::between(measure, abs_limit[1], abs_limit[2])
  }
  if(!is.null(sd_limit)){
    mean_score <- mean(measure[!oor], na.rm=T)
    sd_score <- sd(measure[!oor], na.rm=T)
    lower <- mean_score - sd_limit * sd_score
    upper <- mean_score + sd_limit * sd_score
    oor[!oor] <- !dplyr::between(measure[!oor], lower, upper)
  }
  oor
}
