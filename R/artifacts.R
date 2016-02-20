#' Time Series Artifact Detection and Correction
#'
#' \code{get_artifacts} and \code{fix_artifacts} identify and repair,
#' respectively, signal artifacts within a time series. They are designed to
#' work with pupil measurements obtained from an eye-tracking system, primarily
#' to identify and interpolate over eye blinks, but also any other type of
#' signal artifact that can appear when continuously measuring pupil diameter or
#' area over time.
#'
#' Like similar algorithms, \code{get_artifacts} relies primarily on abrupt
#' jumps in signal to identify the onset and offset of blinks. Unlike other
#' algorithms, it does not require the user to prespecify threshold values that
#' define onsets and offsets for all time series; rather, it adaptively
#' determines the best values based on the distribution of the first-order
#' differences (velocities) of each time series. This procedure was designed to
#' mimic the relativistic way a human observer would visually identify an
#' artifact, i.e., by assessing the pattern of deviation for the candidate
#' artifact relative to that of its nearest neighbors. However, this function
#' can also accommodate the traditional approach by assigning static thresholds
#' via optional parameters.
#'
#' Using the output of \code{get_artifacts}, \code{fix_artifacts} repairs
#' artifacts using a sequence of linear interpolation for internal artifacts
#' (artifacts sandwiched between good data) followed by matched lag-1
#' differences for external artifacts (artifacts at the start or end of a time
#' series).
#'
#' @section Advice on setting \code{lim} argument:
#' You are required to specify a vector of limits that defines what magnitude of
#' relative change from baseline is plausible for your experiment and your
#' equipment. \code{get_artifacts} uses the \code{lim} argument to catch periods
#' of sustained signal loss or signal drift that would not be caught by looking
#' at velocity alone, e.g., if the signal slowly (rather than abruptly) veers
#' into a range that is implausible given its starting point.
#' \code{fix_artifacts} uses the \code{lim} argument to constrain extrapolations
#' at the beginning and end of trials. Depending on the \code{threshold}
#' argument, \code{fix_artifacts} imposes a floor or ceiling to the
#' extrapolation (when \code{threshold = TRUE}), or rejects the entire time
#' series (when \code{threshold = FALSE}).
#'
#' In setting the \code{lim} argument, think about what sort of relative change
#' is plausible for your measurement. For example, if you are measuring pupils,
#' the normal pupil size in adults varies from a minimum diameter of about 2 mm
#' (3 square mm area) to a maximum diameter of about 8 mm (50 square mm area) in
#' bright light vs. darkness. This means if the pupil starts off at maximum
#' dilation, it can experience at most a 75\% decrease in diameter (95\%
#' reduction in area). If the pupil starts off at minimum dilation, it can
#' experience at most a 300\% increase in diameter (1500\% increase in area).
#' This would correspond to \code{lim = c(-0.75, 3)} for diameter, but you can
#' and should set your limits to be more conservative if you do not expect your
#' measurements to span this range. We have found that with psychological
#' stimuli using our eye-tracking setup, \code{lim = c(-0.5, 0.5)} and
#' \code{lim = c(-.75, 1.25)} appear to provide liberal coverage for plausible
#' changes in pupil diameter and area, respectively.
#'
#' @section Warning:
#' If the time series contains gaps that are too long for interpolation (as
#' defined in the \code{max_gap} argument) or if the extrapolated portion of
#' the time series exceeds the range specified in the \code{lim} argument and
#' \code{threshold} is set to \code{FALSE} (the default), \code{fix_artifacts}
#' will silently return a time series consisting of all missing values so that
#' the user can easily identify which trials need to be dropped.
#'
#' @section Disclaimer:
#' This algorithm is intended for the detection of artifacts in relatively short
#' time series corresponding to pupil-dilation measurements. It has only been
#' tested and validated on 6-8 second trials sampled at 60 Hz or 500 Hz. Good
#' artifact detection may or may not generalize to other sampling rates, trial
#' lengths, and types of data. Please use \code{\link{plot_artifacts}} to
#' inspect the performance of \code{get_artifacts} before continuing with
#' \code{fix_artifacts} and any subsequent data cleaning and analysis.
#'
#' @param ts A time series, passed as a numeric vector of chronologically
#' ordered, positively valued observations separated by equal intervals of time.
#'
#' @param samp_freq Sampling frequency in Hz.
#'
#' @param lim Two-item numeric vector \code{c(neg, pos)} specifying the negative
#' and positive limits, respectively, of relative change from baseline
#' that is plausible for your time series. (See section "Advice on setting
#' \code{lim} argument" for more information and examples.) Values outside of
#' this range will be flagged as artifacts regardless of whether or not they
#' deviate markedly from their nearest neighbors.
#'
#' @param baseline Logical vector indicating which parts of the time series
#' correspond to the baseline period. This is used as a reference for
#' determining whether values exceed the limits of relative change set in the
#' \code{lim} argument. If there is no \code{baseline} provided, the mean of the
#' entire series (excluding signal loss) will be used for this reference.
#'
#' @param max_loss Maximum fraction of time series that is allowed to contain
#' dropped signal and/or artifacts. Default value is 0.5, meaning if more than
#' half of the time series is missing, \code{get_artifacts} will label the
#' entire time series as one continuous artifact, and, if more than half of the
#' time series consists of artifacts, \code{fix_artifacts} will not perform
#' interpolation/extrapolation and instead will return a vector of all
#' \code{NA}s.
#'
#' @param max_gap Maximum gap in seconds that an artifact period is allowed to
#' span. Default value is 1 second.
#'
#' @param min_cont Minimum continuity in seconds required between artifacts.
#' If the period of "good" data between two artifacts is less than this
#' threshold, the two artifacts are merged into one. Default value is 0.1
#' (100 ms).
#'
#' @param max_velocity The maximum plausible change (in measurement units)
#' that can occur from one reading to the next. Differences between consecutive
#' readings that exceed this value are used to identify artifact onsets, and
#' differences within this value over the \code{min_cont} time span are used to
#' identify artifact offsets. If no value is specified, the function will assign
#' this to the 90th percentile of the absolute values of the first-order lag-x
#' differences (where x is the number of samples that best approximates 20 ms
#' given the \code{samp_freq}) pooled across the entire time series.
#'
#' @param threshold Logical value indicating whether or not thresholding to the
#' range specified in \code{lim} should be performed when extrapolating the
#' beginning and ending of a time series (if the time series begins or ends with
#' an artifact). If set to \code{FALSE} (the default), extrapolations that
#' exceed \code{lim} will cause the entire time series to be rejected and a
#' vector of all \code{NA}s to be returned. If set to \code{TRUE}, the lower
#' and upper \code{lim} will be used to impose a floor and ceiling,
#' respectively, on extrapolation.
#'
#' @param artifacts A logical vector of equal length to the time series that
#' provides logical indexing into which entries of the time series correspond
#' to artifacts. This can be obtained by first calling \code{get_artifacts}. If
#' not supplied by the user, then \code{fix_artifacts} will call
#' \code{get_artifacts} itself.
#'
#' @param ... Additional arguments to be passed to \code{get_artifacts} via
#' \code{fix_artifacts}
#'
#' @return  \code{get_artifacts} returns a logical vector that can be used for
#' logical indexing into the time series to identify data artifacts.
#' \code{fix_artifacts} returns a copy of the time series with artifacts and
#' missing data replaced by interpolated values, or a copy of the time series
#' with all values changed to \code{NA} in the event that the artifacts are too
#' numerous (exceed \code{max_loss}) or too continuous (exceed \code{max_gap}).
#'
#' @name artifacts
NULL

#' @export
#' @rdname artifacts

get_artifacts <- function(ts, samp_freq, lim,
                          baseline = NULL,
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
  # check that time series does not have negative values------------------------
  ts[is.na(ts)] <- 0
  if(any(ts < 0)) stop("Time series has negative values.")
  #=============================================================================
  # initialize variables--------------------------------------------------------
  artifact <- vector("logical", length(ts))
  if(is.null(baseline)) baseline <- !artifact
  if(all(ts[baseline] == 0)) return(!artifact)
  min_lim <- mean(ts[baseline & ts > 0]) * (1+lim[1])
  max_lim <- mean(ts[baseline & ts > 0]) * (1+lim[2])
  cleaner_ts <- ts[ts > 0]
  if(is.null(max_velocity)){
    lag <- floor(samp_freq/50)
    max_velocity <- quantile(abs(diff(cleaner_ts, lag = lag)),.9)
  }
  margin <- floor(samp_freq * min_cont)
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

#' @export
#' @rdname artifacts
fix_artifacts = function(ts, samp_freq, lim,
                         baseline = NULL,
                         max_loss = 0.5,
                         max_gap = 1,
                         ...,
                         threshold = FALSE,
                         artifacts = NULL){
  # store the number of observations in the time series as 'n'
  n <- length(ts)
  # compute thresholds for external interpolations
  if(is.null(baseline)){
    baseline <- vector("logical", n)
    baseline <- !baseline
  }
  ts[is.na(ts)] <- 0
  min_lim <- mean(ts[baseline & ts > 0]) * (1+lim[1])
  max_lim <- mean(ts[baseline & ts > 0]) * (1+lim[2])
  # if no logical vector of artifacts has been passed, run get_artifacts
  if(is.null(artifacts)){
    artifacts <- get_artifacts(ts, samp_freq, baseline, lim,
                               max_gap = max_gap, ...)
  }
  # set observations flagged as artifacts to missing
  ts[artifacts] <- NA_real_
  # if the proportion of artifacts exceeds the max_loss threshold, set all
  # values of the time series to missing
  if(sum(artifacts) / length(artifacts) > max_loss) ts[] <- NA_real_
  # calculate the run-lengths of consecutive artifact vs. artifact-free periods
  # across the whole time series
  runs <- rle(artifacts)
  # extract from this the lengths of the artifact periods, i.e., the gaps in
  # signal
  gap <- runs$lengths[runs$values]
  # if any gaps exceed the max_gap threshold, set all values of the time series
  # to missing
  if(any(gap > samp_freq * max_gap)) ts[] <- NA_real_
  # if the entire time series is now missing (i.e., one or more artifact
  # periods was greater than the maximum allowable gap or maximum allowable
  # loss), do not proceed
  if(all(is.na(ts))) return(ts)
  # catch rare circumstance in which the entire time series consists of a
  # single run of valid data sandwiched between two runs of artifacts, which
  # will break the zoo package's na.approx function
  will_break_zoo <- length(runs$values) == 3 && all(runs$values == c(T, F, T))
  # linear interpolation for internal artifacts
  if(!will_break_zoo) ts <- zoo::na.approx(ts, na.rm = F)
  # if there is missing data at end of trial
  if(is.na(ts[n])){
    # h = the length of the final gap in the trial
    h = gap[length(gap)]
    # [n-2h : n-h] = the interval immediately preceding the final gap that is
    # equal in length to the final gap plus 1
    matched_interval = ts[(n-2*h):(n-h)]
    # if the length of the final gap is more than half the length of the entire
    # time series, or if there are any missing values in the matched interval,
    # cancel the interpolation.
    if(h > n/2 || any(is.na(matched_interval))){
      ts[] <- NA_real_
      return(ts)
    }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(matched_interval)
    # and repeat these difference over the missing interval
    for(i in 1:h){
      ts[n-h+i] <- ts[n-h+i-1] + lag1[i]
      # optionally thresholded to these limits:
      if(threshold){
        if(ts[n-h+i] < min_lim) ts[n-h+i] <- min_lim
        if(ts[n-h+i] > max_lim) ts[n-h+i] <- max_lim
      }
    }
  }
  # if there is missing data at start of trial
  if(is.na(ts[1])){
    # h = the length of the initial gap in the trial
    h = gap[1]
    # [h+1 : 2h+1] = the interval immediately following the first gap that is
    # equal in length to the first gap plus 1
    matched_interval = ts[(h+1):(2*h+1)]
    # if the length of the initial gap is more than half the length of the entire
    # time series, or if there are any missing values in the matched interval,
    # cancel the interpolation.
    if(h > n/2 || any(is.na(matched_interval))){
      ts[] <- NA_real_
      return(ts)
    }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(matched_interval)
    # and repeat these differences over the missing interval
    for(i in h:1){
      ts[i] <- ts[i+1] - lag1[i]
      # optionally thresholded to these limits:
      if(threshold){
        if(ts[i] < min_lim) ts[i] <- min_lim
        if(ts[i] > max_lim) ts[i] <- max_lim
      }
    }
  }
  if(!all(dplyr::between(ts, min_lim, max_lim)))
    ts[] <- NA_real_
  return(ts)
}
