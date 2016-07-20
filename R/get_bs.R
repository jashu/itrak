#' Mean bias score
#'
#' \code{get_bs} returns the mean difference between trials of dichotomous
#' type.
#'
#' @param RT A numeric vector of reaction times in chronological order.
#'
#' @param congruent A logical vector equal in length to \code{RT} that indicates
#'  whether the corresponding entry of \code{RT} is a congruent \code{TRUE} or
#'  incongruent \code{FALSE} trial.
#'
#' @return A length-one numeric vector.
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

get_bs <- function(RT, congruent){
  if(length(RT) != length(congruent))
    stop("RT and congruent must contain the same number of trials.")
  if(typeof(congruent) != "logical")
    stop("congruent must be a logical vector (TRUE or FALSE)")
  mean(RT[!congruent], na.rm = T) -
    mean(RT[congruent], na.rm = T)
}
