#' Summarize features from time series of A/B trials
#'
#' \code{tsAB} is for time series containing a mixture of two trial types, as
#' would arise from a within-individual A/B experiment (for example, responses
#' over time to images that are either happy or sad). It is typically used on
#' data grouped by individual time series, created by
#' \code{\link[dplyr]{group_by}}. The output will have one row per time series.
#'
#' @param data A data frame of at least two columns: one indicating the trial
#' \code{type}, which must be dichotomous (e.g., A/B), and one giving the
#' response \code{value}. Rows must be ordered by trial sequence. The data frame
#' may contain multiple time series, but in this case a
#' \code{\link[dplyr]{grouped_df}} must be created by calling the
#' \code{\link[dplyr]{group_by}} method on the variable or variables
#' (usually the participant ID) nesting the individual time series.
#'
#' @param type Name of the column in \code{data} giving the trial type. Must be
#' passed as a string (inside quotation marks).
#'
#' @param value Name of the column in \code{data} giving the observed value
#' for each trial. Must be passed as a string (inside quotation marks).
#'
#' @param pos_thresh a \code{value} greater than this threshold will be counted
#' as a positive response. Default is \code{0}.
#'
#' @param neg_thresh a \code{value} smaller than this threshold will be counted
#' as a negative response. Default is \code{0}.
#'
#' @return An object of the same class as \code{data} reducing each time series
#' to the following summary metrics. Note "A" and "B" will be replaced by the
#' labels given by the \code{type} column.
#' \describe{
#'  \item{trials_total_A}{Total number of trials detected of type A. Useful as
#'   a validity check.}
#'  \item{trials_total_B}{Total number of trials detected of type B. Useful as
#'   a validity check.}
#'  \item{trials_missed_A}{Number of trials of type A with missing data.}
#'  \item{trials_missed_B}{Number of trials of type B with missing data.}
#'  \item{mean_A}{Mean of all trials of type A.}
#'  \item{mean_B}{Mean of all trials of type B.}
#'  \item{sd_A}{Standard deviation of all trials of type A.}
#'  \item{sd_B}{Standard deviation of all trials of type B.}
#'  \item{pos_response_to_A}{Count of type-A trials above threshold set by
#'    \code{pos_thresh}.}
#'  \item{pos_response_to_B}{Count of type-B trials above threshold set by
#'    \code{pos_thresh}.}
#'  \item{neg_response_to_A}{Count of type-A trials below threshold set by
#'    \code{neg_thresh}.}
#'  \item{neg_response_to_B}{Count of type-B trials below threshold set by
#'    \code{neg_thresh}.}
#'  \item{neu_response_to_A}{Count of type-A trials between thresholds set by
#'    \code{neg_thresh} and \code{pos_thresh}.}
#'  \item{neu_response_to_B}{Count of type-B trials between thresholds set by
#'    \code{neg_thresh} and \code{pos_thresh}.}
#'  \item{mean_A_after_A}{Mean of all trials of type A that follow another trial
#'    of type A.}
#'  \item{mean_B_after_B}{Mean of all trials of type B that follow another trial
#'    of type B.}
#'  \item{mean_A_after_B}{Mean of all trials of type A that immediately follow a
#'    trial of type B.}
#'  \item{mean_B_after_A}{Mean of all trials of type B that immediately follow a
#'    trial of type A.}
#'  \item{diff_A_from_B}{Mean difference of all trials of type A from an
#'    immediately preceding trial of type B.}
#'  \item{diff_B_from_A}{Mean difference of all trials of type B from an
#'    immediately preceding trial of type A.}
#'  \item{drift_over_A}{Mean linear slope over consecutive trials of type A.}
#'  \item{drift_over_B}{Mean linear slope over consecutive trials of type B.}
#'  }

#' @examples
#' # Create example data frame containing 10 time series of 100 reaction times
#' # from a dot-probe experiment with trial types (`valence`) of "happy" vs.
#' # "sad".
#' data <- data.frame(id = rep(1:10, each = 100),
#'                    trial = rep(1:100, 10),
#'                    valence = rep(sample(c("happy", "sad"), 1000,
#'                                  replace = TRUE)),
#'                    rt = sample(100:1000, 1000, replace = TRUE),
#'                    congru = sample(c(TRUE,FALSE), 1000, replace = TRUE))
#'
#' # Compute a trial-level bias score for each individual (`group_by(id)`) using
#' # reaction time (`rt`) and logical vectors indicating whether or not the dot
#' # probe was congruent with the target stimulus (`congru`).
#' data <- data %>% group_by(id) %>% mutate(tlbs = get_tlbs(rt, congru)) %>%
#'   ungroup()
#'
#' # Obtain A/B summary features for each individual time series
#' ts_summary <- data %>%
#'   group_by(id) %>%
#'   tsAB(type = "valence", value = "tlbs")
#'
#' @import dplyr
#' @export

tsAB <- function(data, type, value, pos_thresh = 0, neg_thresh = 0){
  if(n_distinct(data[[type]]) != 2) stop("type must have exactly 2 states")
  if(inherits(data, "grouped_df")){
    indices <- attr(data, "indices") %>% map(~ . + 1L)
    result <- map_df(
      indices, ~ tsAB(ungroup(data[.x,]), type, value, pos_thresh, neg_thresh)
    )
    return(bind_cols(attr(data, "labels"), result))
  }
  if(is.factor(data[[type]])) type <- as.character(data[[type]])
  value <- data[[value]]
  run_len <- rle(type)
  trans_trials <- cumsum(run_len$lengths) + 1L
  run_start <- c(1, trans_trials)
  run_start <- run_start[run_start <= length(value)]
  run_end <- cumsum(run_len$lengths)
  run_idx <- map2(run_start, run_end, ~ .x:.y)
  names(run_idx) <- run_len$values
  a <- sort(unique(type))[1]
  b <- sort(unique(type))[2]
  a_after_b <- trans_trials[run_len$values == b]
  a_after_b <- a_after_b[a_after_b <= length(value)]
  a_after_a <- setdiff(which(type == a), a_after_b)
  b_after_a <- trans_trials[run_len$values == a]
  b_after_a <- b_after_a[b_after_a <= length(value)]
  b_after_b <- setdiff(which(type == b), b_after_a)
  a_runs <- run_idx[imap_lgl(run_idx, ~ length(.x) > 1 && .y == a &&
                               !any(is.na(value[.x])))]
  b_runs <- run_idx[imap_lgl(run_idx, ~ length(.x) > 1 & .y == b &&
                               !any(is.na(value[.x])))]
  result <- data_frame(
    trials_total_A = sum(type == a),
    trials_total_B = sum(type == b),
    trials_missed_A = sum(is.na(value[type == a])),
    trials_missed_B = sum(is.na(value[type == b])),
    mean_A = mean(value[type == a], na.rm = TRUE),
    mean_B = mean(value[type == b], na.rm = TRUE),
    sd_A = sd(value[type == a], na.rm = TRUE),
    sd_B = sd(value[type == b], na.rm = TRUE),
    pos_response_to_A = sum(value[type == a] > pos_thresh, na.rm = TRUE),
    pos_response_to_B = sum(value[type == b] > pos_thresh, na.rm = TRUE),
    neg_response_to_A = sum(value[type == a] < neg_thresh, na.rm = TRUE),
    neg_response_to_B = sum(value[type == b] < neg_thresh, na.rm = TRUE),
    neu_response_to_A = sum(between(value[type == a], neg_thresh, pos_thresh),
                             na.rm = TRUE),
    neu_response_to_B = sum(between(value[type == b], neg_thresh, pos_thresh),
                             na.rm = TRUE),
    mean_A_after_A = mean(value[a_after_a], na.rm = TRUE),
    mean_B_after_B = mean(value[b_after_b], na.rm = TRUE),
    mean_A_after_B = mean(value[a_after_b], na.rm = TRUE),
    mean_B_after_A = mean(value[b_after_a], na.rm = TRUE),
    diff_A_from_B = mean(value[a_after_b] - value[a_after_b - 1], na.rm = TRUE),
    diff_B_from_A = mean(value[b_after_a] - value[b_after_a - 1], na.rm = TRUE),
    drift_over_A = mean(map_dbl(a_runs, ~ coef(lm(value[.x] ~ .x))[2])),
    drift_over_B = mean(map_dbl(b_runs, ~ coef(lm(value[.x] ~ .x))[2]))
  )
  names(result) <- gsub("_A", paste("_", a, sep = ""), names(result))
  names(result) <- gsub("_B", paste("_", b, sep = ""), names(result))
  result
}
