#' Trial-level Bias Score
#'
#' \code{get_tlbs} calculates the trial-level bias score (TL-BS) as described by
#' Zvielli et al. (2015).
#'
#' Attention bias tasks, such as the dot probe, consist of congruent trials
#' (CTs) and incongruent trials (ITs). Traditionally, a bias score is computed
#' by taking the mean of all CTs and subtracting it from the mean of all ITs,
#' i.e., \eqn{bias = IT - CT}. Zvielli et al. (2015) proposed a trial-level bias
#' score (TL-BS), which computes a bias score for every trial by comparing it to
#' the most temporally proximal trial of opposite type. If the \code{method}
#' argument is set to \code{"nearest"}, \code{get_tlbs} implements this
#' nearest-trial method of calcualting TL-BS. By default
#' (\code{method = "weighted"}) \code{get_tlbs} uses an alternative
#' weighted-trials method that calculates the weighted mean of all trials of
#' opposite type, with closer trials weighted more heavily than more distant
#' trials (as a function of the inverse square of trial distance.) To calculate
#' TLBS, each CT is subtracted from the weighted mean of all ITs, and the
#' weighted mean of all CTs is subtracted from each IT.
#'
#' The two methods yield highly similar TL-BS numbers, but the weighted method
#' may be preferable for two reasons: 1) In the event that a trial of one type
#' is equidistant from two trials of opposite type, the nearest-trial method
#' arbitrarily chooses one over the other; under this circumstance, the mean of
#' the two trials may be a more valid point of comparison. 2) The nearest-trial
#' method frequently double-counts the same IT-CT subtraction. For example,
#' consider the sequence IT IT CT CT: under the nearest-trial method, the
#' interior IT-CT pair will result in duplicate TL-BS calculations for these two
#' trials. This double counting results in brief but frequent artifactual
#' periods where the TL-BS time series is completely flat. (See examples below.)
#' Under the weighted method, these calculations will be non-identical because a
#' trial is not subtracted directly from another single trial, but rather from
#' uniquely weighted means of all trial pairs.
#'
#' @references Zvielli A, Bernstein A, Koster EHW. 2015. Temporal dynamics of
#' attentional bias. \emph{Clinical Psychological Science}. 3(5):772-788.
#'
#' @seealso \code{\link{summarize_bias}}, \code{\link{get_bs}}
#'
#' @param RT A numeric vector of reaction times in chronological order.
#'
#' @param congruent A logical vector equal in length to \code{RT} that indicates
#'  whether the corresponding entry of \code{RT} is a congruent (\code{TRUE}) or
#'  incongruent (\code{FALSE}) trial.
#'
#' @param prior_weights Optional numeric vector of prior weights indicating the
#'  relative influence that each trial should have on the calculation of TLBS
#'  and summary metrics. If not provided, all trials are assumed to carry equal
#'  weight.
#'
#' @param method String indicating method to be used to calculate TLBS. The
#'  default method "\code{weighted}" compares each trial to the distance-weighted
#'  mean of all trials of opposite type. If \code{method = "nearest"}, the method
#'  described by Zvielli et al. (2015) is implemented, and each trial is compared
#'  to the single nearest trial of opposite type. See Details.
#'
#' @param search_limit If using \code{weighted = FALSE}, an integer
#'  indicating how many trials to look forward or backward to find a trial of
#'  opposite type. Default value is 5. If no match is found within the
#'  \code{search_limit} of a trial, \code{NA} will be returned for that trial.
#'
#' @param fill_gaps Logical indicating whether missing values in the TLBS time
#'  series should be imputed based on neighboring trials. Default is
#'  \code{FALSE}.
#'
#' @return A vector of trial-level bias scores.
#'
#' @examples
#' # Create example time series of 50 reaction times in ms:
#' trial <- 1:25
#' set.seed(1)
#' rt <- sample(100:1000, 25)
#'
#' # Create example trial types of congruent vs. incongruent for above measures:
#' congruent <- sample(c(TRUE,FALSE), 25, replace = TRUE)
#'
#' # Calculate TL-BS using the default weighted-mean method:
#' weighted <- get_tlbs(RT = rt, congruent = congruent)
#'
#' # Calculate TL-BS using the nearest-trial method:
#' unweighted <- get_tlbs(RT = rt, congruent = congruent, method = "nearest")
#'
#' # Note how the nearest-trial method results in intermittent plateaus because
#' # of duplicated subtractions:
#' par(mfrow = c(2,1))
#' plot(trial, weighted, type = "l",
#'      main = "Weighted-trials method")
#' plot(trial, unweighted, type = "l",
#'      main = "Nearest-trial method")
#' @importFrom stats weighted.mean
#' @export

get_tlbs <- function(RT, congruent, prior_weights = NULL, method = "weighted",
                     search_limit = 5, fill_gaps = FALSE){
  if(length(RT) != length(congruent))
    stop("RT and congruent vectors must contain the same number of trials.")
  if(typeof(congruent) != "logical")
    stop("congruent must be a logical vector (TRUE or FALSE)")
  if(!method %in% c("weighted", "nearest"))
    stop(paste("\"", method, "\" method not supported.",
               "Set method to either \"nearest\" or \"weighted\"."))
  if(is.null(prior_weights)) prior_weights <- rep(1, length(RT))
  min_wt <- min(prior_weights, na.rm = T)
  if(min_wt <= 0) prior_weights <- prior_weights + abs(min_wt) + 1
  prior_weights[is.na(prior_weights)] <- 0
  CT <- RT[congruent]
  IT <- RT[!congruent]
  CT_wt <- prior_weights[congruent]
  IT_wt <- prior_weights[!congruent]
  tlbs <- vector("numeric", length(RT))
  if(method == "weighted"){
    for(i in seq_along(tlbs)){
      before_i <- 1:max(i-1, 1)
      after_i <- min(i+1, length(tlbs)):length(tlbs)
      if(i == 1 || i == length(tlbs)){
        trial_dist <- c(i - before_i, after_i - i)
      } else {
        trial_dist <- c(i - before_i, 0, after_i - i)
      }
      if(congruent[i]){
        wt <- IT_wt / (trial_dist[!congruent])^2
        tlbs[i] <- weighted.mean(IT, wt, na.rm = TRUE) - RT[i]
      } else {
        wt <- CT_wt / (trial_dist[congruent])^2
        tlbs[i] <- RT[i] - weighted.mean(CT, wt, na.rm = TRUE)
      }
    }
  } else{
    for(i in seq_along(tlbs)){
      begin <- i - search_limit
      if(begin < 1) begin <- 1
      end <- i + search_limit
      if(end > length(tlbs)) end <- length(tlbs)
      trials <- begin:end
      candidates <- trials[congruent[begin:end] != congruent[i] &
                             !is.na(RT[begin:end])]
      if(length(candidates) == 0){
        tlbs[i] <- NA_real_
        next
      }
      trial_dist <- abs(i - candidates)
      match <- candidates[which.min(trial_dist)]
      temp_tlbs <- RT[i] - RT[match]
      tlbs[i] <- ifelse(congruent[i], -1*temp_tlbs, temp_tlbs)
    }
  }

  if(fill_gaps){
    tlbs <- zoo::na.approx(tlbs, na.rm = FALSE)
    tlbs <- zoo::na.locf(tlbs, na.rm = FALSE)
    tlbs<- zoo::na.locf(tlbs, na.rm = FALSE, fromLast = TRUE)
  }

  tlbs
}
