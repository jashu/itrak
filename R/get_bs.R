#' Mean bias score
#'
#' \code{get_bs} returns the mean difference between congruent and incongruent
#' trials.
#'
#' @param RT A numeric vector of reaction times in chronological order.
#'
#' @param congruent A logical vector equal in length to \code{RT} that indicates
#'  whether the corresponding entry of \code{RT} is a congruent (\code{TRUE}) or
#'  incongruent (\code{FALSE}) trial.
#'
#' @param prior_weights Optional numeric vector of prior weights indicating the
#'  relative influence that each trial should have on the calculation of
#'  mean bias, in the event that there is reason to weight some trials more than
#'  others when calculating their mean.
#'
#' @return A length-one numeric vector corresponding to the mean, or optionally
#'  weighted mean, bias score.
#'
#' @examples
#' # Create example time series of 10 reaction times in ms:
#' rt <- sample(100:1000, 10)
#'
#' # Create example trial types of congruent vs. incongruent for above measures:
#' congruent <- sample(c(TRUE,FALSE), 10, replace = TRUE)
#'
#' get_bs(rt, congruent)

#'
#' @seealso \code{\link{get_tlbs}} for trial-level bias score
#' @export

get_bs <- function(RT, congruent, prior_weights = NULL){
  if(length(RT) != length(congruent))
    stop("RT and congruent must contain the same number of trials.")
  if(typeof(congruent) != "logical")
    stop("congruent must be a logical vector (TRUE or FALSE)")
  if(is.null(prior_weights)) prior_weights <- rep(1, length(RT))
  min_wt <- min(prior_weights, na.rm = T)
  if(min_wt <= 0) prior_weights <- prior_weights + abs(min_wt) + 1
  prior_weights[is.na(prior_weights)] <- 0
  CT <- RT[congruent]
  IT <- RT[!congruent]
  CT_weights <- prior_weights[congruent]
  IT_weights <- prior_weights[!congruent]

  weighted.mean(IT, IT_weights, na.rm = T) -
    weighted.mean(CT, CT_weights, na.rm = T)
}
