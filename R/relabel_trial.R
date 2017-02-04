#' Relabel trial numbers
#'
#' Assign invalid trials and renumber remaining trials as a consecutive sequence
#' from 1 to the total number of trials.
#'
#'
# Note that the trial number labels in this file are counting drift-correct
# trials. This will pose a problem when you attempt to join this file
# to the artifact counts, which excluded drift-check trials. I added a
# function `relabel_trial` to `itrak` that will renumber the trials,
# assigning missing values to invalid trials. In this case, we label as
# valid a trial that is not missing data for the `lstim` field.


#' @export
relabel_trial <- function(trial, valid){
  if(any(diff(trial) < 0))
    stop("Trials must first be arranged in numerical order")
  trial[!valid] <- NA
  unique_trial_nums <- unique(trial[valid])
  new_trial_nums <- 1:length(unique_trial_nums)
  trial[valid] <- sapply(trial[valid],
                         function(x) new_trial_nums[x == unique_trial_nums])
  trial
}
