#' Label Trial Number
#'
#' Label individual time series according to trial sequence.
#'
#' If your time series vector contains a larger time series that represents a
#' sequence of smaller time series (as would occur in a multi-trial experiment),
#' it is often useful to explicitly represent this meta-sequence in statistical
#' models (e.g., as trial effects). However, your time series data may not
#' represent this variable explicitly, in which case you may need to infer the
#' trial number from other variables. At minimum, your data should contain a
#' sample index, i.e., an integer sequence that resets with each trial.
#' \code{label_trial} uses this information to construct a trial label.
#'
#' \code{label_trial} detects the start of a new trial using the most generic
#' rule possible--when the sample index decreases instead of increasing,
#' increment the trial number--because sometimes this may be the only reliable
#' indicator of trial number that you have. But note that there are more
#' computationally efficient solutions if you know that all of your trials have
#' an equal number of samples (in which case it will be faster to use
#' \code{rep(1:<number of trials>, each = <samples per trial>}) or if trials can
#' be uniquely identified using another variable or combination of variables as
#' a key (in which case it will be faster to label the sequence of keys and
#' then \code{\link[dplyr]{join}} this value to your time-series data).
#'
#' Optionally, an event-onset marker can be included that indicates that a time
#' series belongs to a valid trial. If this onset marker is not detected for a
#' time series, then \code{label_trial} will not include this series in the
#' trial count and will return a label of \code{NA}. This can then be used as a
#' flag for removing extraneous measurements, such as pupil readings taken
#' during a drift check or calibration procedure.
#'
#' @return An integer vector of trial numbers reflecting the temporal order of
#' multiple time series.
#'
#' @param sample Index number for the time series that resets with the start of
#'  a new trial.
#'
#' @param marker Vector containing markers of trial events.
#'
#' @param event Label in \code{marker} that is uniquely associated with valid
#' trials. If this is a character string, it must be enclosed in quotation
#' marks.
#'
#' @export

label_trial <- function(sample, marker = NULL, event = NULL){
  trial <- vector("integer", length(sample))
  trial_counter <- 1
  trial[1] <- 1
  for(i in 2:length(sample)){
    if(sample[i] < sample[i-1]){
      if(!is.null(marker) && !event %in% marker[trial == trial_counter]){
        trial[trial == trial_counter] <- NA_integer_
      } else {
        trial_counter <- trial_counter + 1
      }
    }
    trial[i] <- trial_counter
  }
  if(!is.null(marker) && !event %in% marker[trial == trial_counter]){
    trial[trial == trial_counter] <- NA_integer_
  }
  trial
}
