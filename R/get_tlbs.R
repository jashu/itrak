#' Trial level bias score
#'
#' \code{get_tlbs} matches each trial (in an ordered series of trials of
#' dichotomous type) to the most temporally proximal trial of opposite type
#' and returns the difference.
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
#' @param search_limit How many trials to look forward or backward to find a
#'  trial of opposite type. Default value is 5. If no match is found within the
#'  \code{search_limit} of a trial, \code{NA} will be returned for that trial.
#'
#' @return A vector of bias scores for each trial.
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
#' get_tlbs(measure = rt, type = trial_type)
#'
#' # To subtract congruent trials from incongruent trials, specify "incongruent"
#' # using the reference argument:
#' get_tlbs(rt, trial_type, reference = "incongruent")
#'
#' # Or, if type is converted to a factor variable with "incongruent" as the
#' # reference level, then congruent trials will be subtracted from incongruent
#' # by default:
#' trial_type <- factor(trial_type, levels = c("incongruent", "congruent"))
#' get_tlbs(rt, trial_type)
#'
#' # Calculate the bias score using the nearest trial within 3 trials:
#' get_tlbs(rt, trial_type, search_limit = 3)
#'
#' @seealso \code{\link{get_bs}} for mean bias score
#' @export

get_tlbs <- function(measure, type, reference = NULL, search_limit = 5){
  if(length(measure) != length(type))
    stop("Measure and type must contain the same number of trials.")
  if(length(unique(na.omit(type))) != 2)
    stop("There must be exactly two types of trials.")
  if(is.null(reference))
    reference <- ifelse(is.factor(type), levels(type)[1], sort(unique(type))[1])
  tlbs <- vector("numeric", length(measure))
  for(i in seq_along(tlbs)){
    begin <- i - search_limit
    if(begin < 1) begin <- 1
    end <- i + search_limit
    if(end > length(tlbs)) end <- length(tlbs)
    trials <- begin:end
    candidates <- trials[type[begin:end] != type[i]]
    if(length(candidates) == 0){
      tlbs[i] <- NA_real_
      next
    }
    trial_dist <- abs(i - candidates)
    match <- candidates[which.min(trial_dist)]
    temp_tlbs <- measure[i] - measure[match]
    tlbs[i] <- ifelse(type[i] == reference, temp_tlbs, -1*temp_tlbs)
  }
  return(tlbs)
}
