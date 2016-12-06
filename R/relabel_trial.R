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
