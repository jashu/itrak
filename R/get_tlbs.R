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
#' the most temporally proximal trial of opposite type. If the \code{weighted}
#' argument is changed to \code{FALSE}, \code{get_tlbs} implements this
#' nearest-trial method of calcualting TL-BS. By default
#' (\code{weighted = TRUE}) \code{get_tlbs} uses an alternative weighted-trials
#' method. For the first and last 2 IT-CT pairs, the methods are equivalent, but
#' for all trials in between the weighted method calculates the mean of the
#' preceding and subsequent trials of opposite type, with the closer trial
#' weighted more heavily than the more distant trial. This is computed by
#' constructing a CT times series with missing values in place of ITs (and vice
#' versa), performing a linear interpolation over the missing values, and then
#' subtracting the approximated complete CT times series from the approximated
#' complete IT time series. Thus, each real CT is subtracted from a
#' \emph{projected} IT that lies on an imaginary line connecting the two
#' surrounding real ITs, and a similarly projected CT is subtracted from each
#' real IT.
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
#' the weighted means of unique trial pairs. In the above example, the interior
#' CT would not be subtracted from the preceding IT, but rather from the
#' weighted average of the preceding IT and whatever IT comes next.
#'
#' @references Zvielli A, Bernstein A, Koster EHW. 2015. Temporal dynamics of
#' attentional bias. \emph{Clinical Psychological Science}. 3(5):772-788.
#'
#' @seealso \code{\link{summarize_bias}}, \code{\link{get_bs}}
#'
#' @param RT A numeric vector of reaction times in chronological order.
#'
#' @param congruent A logical vector equal in length to \code{RT} that indicates
#'  whether the corresponding entry of \code{RT} is a congruent \code{TRUE} or
#'  incongruent \code{FALSE} trial.
#'
#' @param weighted A logical value. If \code{TRUE} (the default), each congruent
#'  trial is subtracted from the weighted mean of \emph{both} the preceding
#'  \emph{and} subsequent incongruent trials (with the closer of the two trials
#'  receiving the stronger weight). If \code{FALSE}, the method described by
#'  Zvielli et al. (2015) is implemented, and each congruent trial is subtracted
#'  from the single nearest incongruent trial (\emph{either} the preceding
#'  \emph{or} subsequent trial).
#'
#' @param search_limit If using \code{method = "discrete"}, an integer
#'  indicating how many trials to look forward or backward to find a trial of
#'  opposite type. Default value is 5. If no match is found within the
#'  \code{search_limit} of a trial, \code{NA} will be returned for that trial.
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
#' unweighted <- get_tlbs(RT = rt, congruent = congruent, weighted = FALSE)
#'
#' # Note how the nearest-trial method results in intermittent plateaus because
#' # of duplicated subtractions:
#' par(mfrow = c(2,1))
#' plot(trial, weighted, type = "l",
#'      main = "Weighted-trials method")
#' plot(trial, unweighted, type = "l",
#'      main = "Nearest-trial method")

#' @export

get_tlbs <- function(RT, congruent, weighted = TRUE, search_limit = 5){
  if(length(RT) != length(congruent))
    stop("RT and congruent vectors must contain the same number of trials.")
  if(typeof(congruent) != "logical")
    stop("congruent must be a logical vector (TRUE or FALSE)")
  tlbs <- vector("numeric", length(RT))
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

  if(weighted){
    IT <- RT; CT <- RT
    IT[congruent] <- NA_real_
    CT[!congruent] <- NA_real_
    IT <- zoo::na.approx(IT, na.rm = FALSE)
    IT <- zoo::na.locf(IT, na.rm = FALSE)
    IT <- zoo::na.locf(IT, na.rm = FALSE, fromLast = TRUE)
    CT <- zoo::na.approx(CT, na.rm = FALSE)
    CT <- zoo::na.locf(CT, na.rm = FALSE)
    CT <- zoo::na.locf(CT, na.rm = FALSE, fromLast = TRUE)
    tlbs[!is.na(tlbs)] <- (IT - CT)[!is.na(tlbs)]
  }
  return(tlbs)
}
