#' Summary of attention bias metrics
#'
#' \code{summarize_bias} combines the \code{\link{get_bs}} and
#' \code{\link{get_tlbs}} functions to generate a traditional attention bias
#' metric as well as summary metrics of trial-level attention bias
#' (Zvielli et al., 2015).
#'
#' @section IMPORTANT:
#' Before calling this function, ensure that the data being passed have been
#' grouped into sets of trials, with each set containing a single series of
#' measurements belonging to a single individual. \code{\link[dplyr]{group_by}}
#' is recommended for this purpose. If you receive an output that has less or
#' more rows of observations than you were expecting, incorrect grouping is
#' likely to blame.
#'
#' @section SUPER IMPORTANT:
#' You must also ensure that each set of trials is in chronological order.
#' Otherwise, the trial-level bias calculation will be wrong, and there will be
#' no obvious sign that this has happened. To ensure proper grouping and
#' ordering, it is recommended that prior to calling this function you sort your
#' data using \code{\link[dplyr]{arrange}(data, g1, ..., trial)} where \code{g1}
#' is your primary grouping variable (most likely `subject` or `id`) followed by
#' any secondary grouping variables (e.g., 'session', 'category') and ending with
#' the variable that gives the \code{trial} number. This should then be piped to
#' \code{\link[dplyr]{group_by}(g1, ...)}, where \code{g1, ...} corresponds
#' precisely to the same grouping variables used in the call to
#' \code{\link[dplyr]{arrange}}. (Do NOT include \code{trial} in the call to
#' \code{\link[dplyr]{group_by}}.) The result is then ready to be piped to the
#' \code{summarize_bias} function.
#'
#' @references Zvielli A, Bernstein A, Koster EHW. 2015. Temporal dynamics of
#' attentional bias. \emph{Clinical Psychological Science}. 3(5):772-788.
#'
#' @param data A data frame or table.
#'
#' @param measure The name of the column in \code{data} that contains
#' chronologically ordered observations of numeric type.
#'
#' @param trial_type The name of the column in \code{data} that provides the
#'  trial type of the corresponding row of \code{measure}. \code{trial_type}
#'  must be a dichotomous factor (having exactly 2 levels) or convertible to
#'  such.
#'
#' @param reference Value indicating which level of trial \code{type} should
#'  come first when subtracting trials of opposite type to generate a bias
#'  score: \eqn{bias = reference type - opposite type}. If \code{type} is of
#'  class \code{factor}, defaults to the first factor level; otherwise defaults
#'  to the trial type that comes first alphanumerically.
#'
#' @param search_limit When calculating trial-level bias, how many trials to
#'  look forward or backward to find a trial of opposite type. Default value is
#'  5. If no match is found within the \code{search_limit} of a trial, \code{NA}
#'  will be assigned to that trial.
#'
#' @return An object of the same class as \code{data} with the following
#' summary metrics:
#' \describe{
#'  \item{mean_bias}{Traditional bias score obtained by taking the mean of one
#'  trial type and subtracting it from the mean of the opposite trial type.}
#'  \item{mean_toward}{Mean bias toward the target stimulus obtained by
#'  calculating the mean of the positive trial-level bias scores.}
#'  \item{mean_away}{Mean bias away from the target stimulus obtained by
#'  calculating the mean of the negative trial-level bias scores.}
#'  \item{max_toward}{Maximum bias toward the target stimulus obtained by
#'  calculating the maximum trial-level bias score.}
#'  \item{max_away}{Maximum bias away from the target stimulus obtained by
#'  calculating the minimum (most negative) trial-level bias score.}
#'  \item{variance}{Variability in bias obtained by calculating the mean of the
#'  absolute value of the lag-1 differences in trial-level bias scores.}
#'  \item{trials_toward}{Number of trials during which bias was directed toward
#'  the target.}
#'  \item{trials_away}{Number of trials when bias was directed away from the
#'  target.}
#'  \item{trials_NA}{Number of trials for which a trial-level bias score could
#'  not be computed, i.e., a trial of opposite type could not be found within
#'  the \code{search_limit}}
#'  \item{trials_total}{Total number of trials.}
#'  }
#'
#' @examples
#' # Create example data frame containing 10 time series of 10 reaction times
#' # and trial types of congruent ('con') vs. incongruent ('incon'):
#' data <- data.frame(id = rep(1:10, each = 10),
#'                    rt = sample(500:5000, 100),
#'                    type = sample(c("con","incon"), 100, replace = TRUE),
#'                    trial = rep(1:10, 10))
#'
#' # Use dplyr to sort by trial and group by id and then generate bias summary
#' # specifying that congruent trials should be subtracted from incongruent by
#' # setting reference to "incon":
#' data %>%
#' arrange(id, trial) %>%
#' group_by(id) %>%
#' summarize_bias(measure = rt, trial_type = type, reference = 'incon')
#'
#' @seealso \code{\link{get_bs}}, \code{\link{get_tlbs}},
#' \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{summarize}}
#'
#' @export

summarize_bias <- function(data, measure, trial_type, reference = NULL,
                           search_limit = 5){
  a <- as.list(match.call())
  data$measure <- eval(a$measure, data)
  data$trial_type <- eval(a$trial_type, data)
  data <- dplyr::mutate(data, tlbs = get_tlbs(measure, trial_type, reference,
                                               search_limit))
  dplyr::summarize(data,
                   mean_bias = get_bs(measure, trial_type, reference),
                   mean_toward = mean(tlbs[tlbs > 0], na.rm = T),
                   mean_away = mean(tlbs[tlbs < 0], na.rm = T),
                   max_toward = max(tlbs, na.rm = T),
                   max_away = min(tlbs, na.rm = T),
                   variance = mean(abs(diff(tlbs)), na.rm = T),
                   trials_toward = sum(!is.na(tlbs[tlbs > 0])),
                   trials_away = sum(!is.na(tlbs[tlbs < 0])),
                   trials_NA = sum(is.na(tlbs)),
                   trials_total = n())
}
