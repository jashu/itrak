#' Mean bias score
#'
#' \code{get_bs} returns the mean difference between trials of dichotomous
#' type.
#'
#' @param measure A vector of chronologically ordered observations of numeric
#'  type.
#'
#' @param type A vector of equal length to \code{measure} that provides the
#'  trial type of the corresponding entry of \code{measure}. Must be a
#'  dichotomous factor (having exactly 2 levels) or convertible to such.
#'
#' @param reference Value indicating which level of trial \code{type} should
#'  come first when subtracting trials of opposite type to generate a bias score
#'  \eqn{bias = reference type - opposite type}. If \code{type} is of class
#'  \code{factor}, defaults to the first level; otherwise defaults to the trial
#'  type that comes first alphanumerically.
#'
#' @return A length-one numeric vector.
#'
#' @examples
#' # Create example time series of 10 reaction times in ms:
#' rt <- sample(1000:10000, 10)
#'
#' # Create example trial types of congruent vs. incongruent for above measures:
#' trial_type <- sample(c("congruent","incongruent"), 10, replace = TRUE)
#'
#' # By default, "congruent" is treated as the reference because of alphabetical
#' # order, so incongruent trials will be subtracted from congruent trials:
#' get_bs(measure = rt, type = trial_type)
#'
#' # To subtract congruent trials from incongruent trials, specify "incongruent"
#' # using the reference argument:
#' get_bs(rt, trial_type, reference = "incongruent")
#'
#' # Or, if type is converted to a factor variable with "incongruent" as the
#' # reference level, then congruent trials will be subtracted from incongruent
#' # by default:
#' trial_type <- factor(trial_type, levels = c("incongruent", "congruent"))
#' get_bs(rt, trial_type)
#'
#' @seealso \code{\link{get_tlbs}} for trial-level bias score
#' @export

get_bs <- function(measure, type, reference = NULL){
  if(length(measure) != length(type))
    stop("Measure and type must contain the same number of trials.")
  if(length(unique(na.omit(type))) != 2)
    stop("There must be exactly two types of trials.")
  if(is.null(reference))
    reference <- ifelse(is.factor(type), levels(type)[1], sort(unique(type))[1])
  mean(measure[type == reference], na.rm = T) -
    mean(measure[type != reference], na.rm = T)
}
