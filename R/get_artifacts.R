#' Artifact detection
#'
#' \code{get_artifacts} returns a logical vector indicating the location of all
#' artifacts present in a pupil-diameter time series.
#'
#' This algorithm is an extension of other blink-detection algorithms and seeks
#' to identify all artifacts--not just blinks--that can appear when continuously
#' measuring pupil diameter over time.
#'
#' Like other algorithms, this one relies primarily on abrupt jumps in signal to
#' identify the onset and offset of blinks. Unlike other algorithms, it does not
#' require the user to prespecify threshold values that define onsets and
#' offsets for all time series; rather, it adaptively determines the best values
#' based on the distribution of the first-order differences (velocities) of each
#' time series. This procedure was designed to mimic the relativistic way a
#' human observer would visually identify an artifact, i.e., by assessing the
#' pattern of deviation for the candidate artifact relative to that of its
#' neighbors. However, this function can also accommodate the traditional
#' approach by assigning static thresholds via optional parameters.
#'
#' @section Disclaimer:
#' This algorithm is intended for the detection of artifacts in relatively short
#' time series corresponding to pupil-dilation measurements. It has only been
#' tested and validated on 8-second trials sampled at 60 Hz or 500 Hz. Good
#' artifact detection may or may not generalize to other sampling rates, trial
#' lengths, and types of data.
#'
#' @param ts A time series, passed as a vector of chronologically ordered
#' observations separated by equal intervals of time.
#'
#' @param samp_freq Sampling frequency in Hz.
#'
#' @param baseline Logical vector indicating which parts of the time series
#' correspond to the baseline period. This is used as a reference for
#' determining whether values exceed the limits of relative change set in the
#' \code{lim} argument. If there is no baseline, the mean of the entire series
#' (excluding signal loss) will be used for this reference.
#'
#' @param lim Vector of limits specifying minimum and maximum relative change
#' from baseline that is plausible for your experiment. Default is
#' \code{c(-.5,.5)}, meaning values that represent more than a 50\% decrease or
#' 50\% increase from baseline will be considered artifacts.
#'
#' @param max_loss Maximum fraction of total signal loss that is tolerable.
#' Default value is 0.5, meaning time series with more than 50\% signal loss will
#' be rejected.
#'
#' @param max_gap Maximum gap in seconds that a loss of signal is allowed to
#' span. Default value is 1 second.
#'
#' @param min_cont Minimum continuity in seconds required between artifacts.
#' If the period of "good" data between two artifacts is less than this
#' threshold, the two artifacts are merged into one. Default value is 0.1
#' (100 ms).
#'
#' @param max_velocity The maximum plausible change (in absolute diameter units)
#' that can occur from one reading to the next. Differences between consecutive
#' readings that exceed this value are used to identify artifact onsets, and
#' differences within this value over the \code{min_cont} time span are used to
#' identify artifact offsets. If no value is specified, the function will assign
#' this to the 90th percentile of the absolute values of the first-order lag-1
#' differences pooled across the entire time series.
#'
#' @return A logical vector that can be used for logical indexing into the time
#' series to identify data artifacts.
#'
#' @export

get_artifacts <- function(ts, samp_freq, baseline = NULL,
                          lim = c(-0.5, 0.5),
                          max_loss = 0.5,
                          max_gap = 1,
                          min_cont = 0.1,
                          max_velocity = NULL){
  #============================================================================
  # local helper functions
  #----------------------------------------------------------------------------
  ## determine if array index is within a valid range
  is_valid_index <- function(i){
    i >= 1 && i <= length(ts)
  }
  ## determine if section of time series is stable
  is_stable <- function(period){
    all(period > min_lim & period < max_lim) &&
      all(abs(diff(period)) <= max_velocity)
  }
  ## eliminate "data islands" between artifacts (periods less than min_cont)
  merge_artifacts <- function(artifacts){
    runs <- rle(artifacts)
    island <- which(runs$lengths < margin & !runs$values)
    if(length(island) > 0){
      for (i in seq_along(island)){
        start_index <- sum(runs$lengths[0:(island[i] - 1)]) + 1
        end_index <- sum(runs$lengths[1:island[i]])
        artifacts[start_index:end_index] <- TRUE
      }
    }
    return(artifacts)
  }
  #=============================================================================
  # initialize variables--------------------------------------------------------
  artifact <- vector("logical", length(ts))
  if(is.null(baseline)) baseline <- !artifact
  ts[is.na(ts)] <- 0
  margin <- floor(samp_freq * min_cont)
  if(all(ts[baseline] == 0)) return(!artifact)
  min_lim <- mean(ts[baseline & ts > 0]) * (1+lim[1])
  max_lim <- mean(ts[baseline & ts > 0]) * (1+lim[2])
  cleaner_ts <- ts[ts > 0]
  if(is.null(max_velocity)){
    lag <- floor(samp_freq/50)
    max_velocity <- quantile(abs(diff(cleaner_ts, lag = lag)),.9)
  }

  #=============================================================================
  # check to see if running algorithm is worthwhile
  #-----------------------------------------------------------------------------
  # if it is already clean, accept time series as-is
  if(is_stable(ts)) return(artifact)
  # if signal loss is excessive, reject entire time seires
  gap <- rle(ts)
  gap <- gap$lengths[gap$values==0] / samp_freq
  if(any(gap > max_gap) || sum(ts == 0) > max_loss * length(ts))
    return(!artifact)
  #=============================================================================
  # artifact-detection algorithm
  #-----------------------------------------------------------------------------
  is_artifact <- function(forward){
    i <- ifelse(forward, 1, length(ts))
    margin <- ifelse(forward, margin, -margin)
    # if first observations are not valid or stable, keep marking observations
    # as artifacts until the first valid, stable observations are found
    stable <- is_stable(ts[i:(i+margin)])
    while(is_valid_index(i + margin) && !stable){
      stable <- is_stable(ts[i:(i+margin)])
      artifact[i:(i + margin)] <- !stable
      i <- ifelse(forward,i+1,i-1)
    }
    if(all(artifact)) return(artifact)
    # look for artifact onset
    i <- ifelse(forward,i+1,i-1)
    while(is_valid_index(i)){
      onset <- NULL
      offset <- NULL
      delta <- ts[i] - ifelse(forward,ts[i-1],ts[i+1])
      if(ts[i] == 0 || abs(delta) > max_velocity)
        onset <- i
      # look for offset
      stable <- FALSE; reversed <- FALSE
      counter <- 0; rising <- delta > 0
      while(is_valid_index(i) && !is.null(onset) && is.null(offset)){
        counter <- counter + 1
        delta <- ts[i] - ifelse(forward,ts[i-1],ts[i+1])
        reversed <- ifelse(rising, delta < 0, delta > 0)
        if(reversed){
          i <- ifelse(forward, i + counter, i - counter)
          if(is_valid_index(i)) offset <- i
        } else {
        if(is_valid_index(i + margin/2) && is_valid_index(i - margin/2) &&
             is_stable(ts[(i - margin/2):(i + margin/2)])) offset <- i
        }
        if(is.null(offset)) i <- ifelse(forward,i+1,i-1)
      }
      if(!is.null(onset)){
        if(is.null(offset)) offset <- ifelse(forward,length(ts),1)
        artifact[onset:offset] <- TRUE
      }
      i <- ifelse(forward,i+1,i-1)
    }
    return(artifact)
  }
  # =====================================================================
  artifact <- is_artifact(forward = TRUE) | is_artifact(forward = FALSE)
  return(merge_artifacts(artifact))
}
