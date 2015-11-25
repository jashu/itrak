#' Valid trials
#'
#' \code{is_valid} indicates which trials fall within a valid range of values.
#'
#' @param measure A vector of chronologically ordered observations of numeric
#'  type.
#'
#' @param limit A two-item numeric vector \code{c(lower, upper)} specifying the
#'  lower and upper limits, respectively, of the range of valid values. The
#'  range can be specified in terms of either "raw" or "sd" (standard deviation)
#'  values, or a mixture of the two as defined by the \code{type} argument.
#'
#' @param type A one- or two-item vector corresponding to the type of unit
#'  ("raw" or "sd") of the \code{limit} argument. If the lower and upper
#'  \code{limit} are specified in different units, then this argument should
#'  correspond to \code{c(type_lower, type_upper)}.
#'
#' @return A logical vector indicating \code{TRUE} if the trial is valid and
#'  \code{FALSE} if it is not.
#'
#' @examples
#' # Create example time series of 10 reaction times in ms with two trials that
#' # fall outside the bounds of validity:
#' rt <- c(0, 20000, sample(1000:5000, 8))
#'
#' # Check that trials are not less than 250 ms and not more than 2.5 standard
#' # deviations above the mean:
#' is_valid(rt, limit = c(250, 2.5), type = c("raw","sd"))
#'
#' @export

is_valid <- function(measure, limit, type){
  if(!is.numeric(measure))
    stop("The measure is not stored as a numeric type.")
  if(length(limit) != 2)
    stop("The limit must be given as a two-element vector.")
  if(!is.numeric(limit))
    stop("The limit must be given in numbers.")
  if(!length(type) %in% c(1,2))
    stop("Type must be a single character string or a vector of two strings.")
  if(!all(type %in% c("raw","sd")) )
    stop("'raw' and 'sd' are the only recognized types.")
  if(length(type) == 1) type <- c(type, type)
  mean_score <- mean(measure, na.rm=T)
  sd_score <- sd(measure, na.rm=T)
  lower <- ifelse(type[1] == "raw", limit[1], mean_score - limit[1] * sd_score)
  upper <- ifelse(type[2] == "raw", limit[2], mean_score + limit[2] * sd_score)
  measure > lower & measure < upper
}
