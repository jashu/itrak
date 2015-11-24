#' Trial level bias score
#'
#' \code{get_tlbs} Given an ordered series of trials of dichotomous type,
#' each trial is matched with the trial of opposite type that is most temporally
#' proximal, and the difference score is returned.
#'
#' @param measure A vector of chronologically ordered observations of numeric
#' type.
#'
#' @param type A vector of equal length to \code{measure} that provides the
#' trial type of the corresponding entry of \code{measure}. Must be a
#' dichotomous factor (having exactly 2 levels) or convertible to such.
#'
#' @param reference Value indicating which level of trial \code{type} should
#' come first when subtracting trials of opposite type to generate a bias score
#' (bias = \code{reference} type - opposite type). If \code{type} is of class
#' \code{factor}, defaults to the first level; otherwise defaults to the trial
#' type that comes first alphanumerically.
#'
#' @param search_limit How many trials to look forward or backward to find a
#' trial of opposite type. Default value is 5. If no match is found within the
#' \code{search_limit} of a trial, \code{NA} will be returned for that trial.
#'
#' @return A vector of bias scores for each trial.
#'
#' @export

get_tlbs <- function(measure, type, reference = NULL, search_limit = 5){
  if(length(measure) != length(length(type)))
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
    if(end > length(tlbs)) end = length(tlbs)
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
