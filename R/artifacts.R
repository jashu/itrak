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
#' artifact relative to that of its nearest neighbors.
#'
#' Using the output of \code{get_artifacts}, \code{fix_artifacts} repairs
#' artifacts using a sequence of linear interpolation for internal artifacts
#' (artifacts sandwiched between good data) followed by matched lag-1
#' differences for external artifacts (artifacts at the start or end of a time
#' series).
#'
#' @section Advice on setting \code{lim} argument:
#'
#' \code{fix_artifacts} uses the \code{lim} argument to impose a floor and
#' ceiling to the values that the time series is allowed to take. Values that
#' exceed this range are then treated like any other artifact. The purpose of
#' the \code{lim} argument is to catch time series that contain "slow drift",
#' meaning the signal gradually drifts into implausible values. In setting the
#' \code{lim} argument, think about what sort of relative change is plausible
#' for your measurement. For example, if you are measuring pupils, the normal
#' pupil size in adults varies from a minimum diameter of about 2 mm (3 square
#' mm area) to a maximum diameter of about 8 mm (50 square mm area) in bright
#' light vs. darkness. This means if the pupil starts off at maximum dilation,
#' it can experience at most a 75\% decrease in diameter (95\% reduction in
#' area). If the pupil starts off at minimum dilation, it can experience at most
#' a 300\% increase in diameter (1500\% increase in area). This would correspond
#' to \code{lim = c(-0.75, 3)} for diameter and \code{lim = c(-0.95, 1500)} for
#' area, but you should set your limits to be more conservative if you do not
#' expect your measurements to span this range.
#'
#' For example, most users will measure pupils under moderate lighting
#' conditions, so baseline pupil readings will start off closer to the center of
#' their physiological range. Even assuming maximum decreases and increases from
#' this point, the range could be narrowed to \code{lim = c(-0.6, 0.6)} for
#' diameter and \code{lim = c(-0.85, 1.5)} for area. We have found that with
#' psychological stimuli using our eye-tracking setup, \code{lim = c(-0.5, 0.5)}
#' and \code{lim = c(-.75, 1.25)} appear to provide liberal coverage for
#' plausible changes in pupil diameter and area, respectively, and you want your
#' \code{lim} setting to err on the side of being too wide. Note that
#' \code{fix_artifacts} calls the \code{get_oor} ("oor" for "out of range")
#' function to identify which periods of the time series violate the \code{lim}
#' argument. If you wish to know which periods are out of range, you can also
#' call this function directly.
#'
#' @section Warning:
#' If the time series contains periods of artifacts that are too long for
#' interpolation defined by the \code{max_gap} argument, or if the total
#' number of artifacts exceeds the proportion specified by the \code{max_loss}
#' argument, \code{fix_artifacts} will silently return a time series consisting
#' of all missing values so that the user can easily identify which trials need
#' to be dropped.
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
#' @seealso \code{\link{plot_artifacts}},  \code{\link{plot_comparison}},
#'  \code{\link{normalize}},  \code{\link{low_pass_filter}}
#'
#' @param ts A time series, passed as a numeric vector of chronologically
#' ordered, positively valued observations separated by equal intervals of time.
#'
#' @param samp_freq Sampling frequency in Hz.
#'
#' @param max_velocity Maximum allowable velocity specified in terms of a
#' quantile of the distribution of absolute values of first-order differences of
#' the time series. Velocities that exceed the value of this quantile will be
#' used to identify onset/offset of artifacts. Default value is 0.9 (90th
#' percentile). Lower values will lead to greater sensitivity but less
#' specificity in identifying signal spikes. Higher values will lead to less
#' sensitivity but greater specificity.
#'
#' @param lim Two-item numeric vector \code{c(neg, pos)} specifying the negative
#' and positive limits, respectively, of relative change from baseline
#' that is plausible for your time series. (See section "Advice on setting
#' \code{lim} argument" for more information and examples.)
#'
#' @param baseline Logical vector indicating which parts of the time series
#' correspond to the baseline period. This is used as a reference for
#' determining whether values exceed the limits of relative change set in the
#' \code{lim} argument. If there is no \code{baseline} provided, the mean of the
#' entire series (excluding signal loss) will be used for this reference.
#'
#' @param max_loss Maximum fraction of time series that is allowed to contain
#' dropped signal and/or artifacts. Default value is 0.5, meaning if more than
#' half of the time series consists of artifacts, \code{fix_artifacts} will not
#' perform interpolation/extrapolation and instead will return a vector of all
#' \code{NA}s.
#'
#' @param max_gap Maximum gap in seconds that an artifact period is allowed to
#' span. Default value is 1 second, meaning if more than half of the time series
#' consists of artifacts, \code{fix_artifacts} will not perform
#' interpolation/extrapolation and instead will return a vector of all
#' \code{NA}s.
#'
#' @param min_cont Minimum continuity in seconds required between artifacts.
#' If the period of "good" data between two artifacts is less than this
#' threshold, the two artifacts are merged into one. Default value is 0.2
#' (200 ms).
#'
#' @param artifacts A logical vector of equal length to the time series that
#' provides logical indexing into which entries of the time series correspond
#' to artifacts. This can be obtained by first calling \code{get_artifacts}. If
#' not supplied by the user, then \code{fix_artifacts} will call
#' \code{get_artifacts} itself.
#'
#' @param ...	further arguments passed to \code{get_artifacts} from
#' \code{get_oor} or \code{fix_artifacts} if it has not already been used to
#' create an \code{artifacts} vector.
#'
#' @return  \code{get_artifacts} returns a logical vector that can be used for
#' logical indexing into the time series to identify data artifacts.
#' \code{get_oor} returns a logical vector corresponding to elements of the time
#' series that are out of range, as defined by amount of relative change from
#' baseline using the \code{lim} and \code{baseline} arguments.
#' \code{fix_artifacts} returns a copy of the time series with artifacts and
#' missing data replaced by interpolated values, or a copy of the time series
#' with all values changed to \code{NA} in the event that the artifacts are too
#' numerous (exceed \code{max_loss}) or too continuous (exceed \code{max_gap}).
#'
#' @name artifacts
NULL

#' @importFrom stats quantile
#' @export
#' @rdname artifacts

get_artifacts <- function(ts, samp_freq, min_cont = 0.2, max_velocity = 0.9){
  #=============================================================================
  # check that time series does not have negative values
  #-----------------------------------------------------------------------------
  ts[is.na(ts)] <- 0
  if(any(ts < 0)) stop("Time series has negative values.")
  #=============================================================================
  # initialize variables
  #-----------------------------------------------------------------------------
  artifact <- vector("logical", length(ts))
  lag <- floor(samp_freq/50)
  max_v <- quantile(abs(diff(ts[ts > 0], lag = lag)), max_velocity)
  margin <- floor(samp_freq * min_cont)

  #=============================================================================
  # artifact-detection algorithm
  #-----------------------------------------------------------------------------
  artifact[ts == 0] <- TRUE; forward <- artifact; backward <- artifact
  forward[2:length(ts)] <- abs(diff(ts)) > max_v
  backward[(length(ts)-1):1] <- abs(diff(ts[length(ts):1])) > max_v
  artifact <- merge_artifacts(artifact | forward | backward, margin)
  artifact <- buffer_artifacts(ts, artifact, margin)
  artifact <- merge_artifacts(artifact, margin)

  #=============================================================================
  # if there are any artifacts, run the algorithm again on the corrected ts
  #-----------------------------------------------------------------------------
  if(all(artifact) || all(!artifact)) return(artifact)
  new_ts <- fix_artifacts(ts, samp_freq, lim = c(-.99, Inf),
                          artifacts = artifact, min_cont = min_cont,
                          max_gap = Inf, max_loss = 1)
  if(all(is.na(new_ts))) return(artifact)
  artifact <- artifact |
    get_artifacts(new_ts, samp_freq, min_cont, max_velocity)
  merge_artifacts(artifact, margin)
}

#' @importFrom stats median
#' @export
#' @rdname artifacts

get_oor <- function(ts, samp_freq, lim, ..., min_cont = 0.2, baseline = NULL,
                    artifacts = NULL){
  # if no logical vector of artifacts has been passed, run get_artifacts
  if(is.null(artifacts)){
    artifacts <- get_artifacts(ts, samp_freq, ..., min_cont = min_cont)
  }
  # obtain cleaned time series without any constraints on max_loss or max_gap
  new_ts <- fix_artifacts(ts = ts, samp_freq = samp_freq, baseline = baseline,
                          artifacts = artifacts, max_gap = Inf, max_loss = 1)
  if(all(is.na(new_ts))){
    ts[artifacts] <- NA_real_
  } else {
    ts <- new_ts
  }
  # determine absolute limits based on relative limits and baseline period
  if(is.null(baseline)) baseline <- !is.na(ts)
  min_lim <- median(ts[baseline]) * (1+lim[1])
  max_lim <- median(ts[baseline]) * (1+lim[2])
  margin <- floor(samp_freq * min_cont)
  # return logical test for which parts of time series exceed lim argument
  oor <- !artifacts & (ts < min_lim | ts > max_lim)
  oor <- merge_artifacts(oor, margin)
  oor <- buffer_artifacts(ts, oor, margin)
  merge_artifacts(oor, margin)
}

#' @importFrom stats median
#' @export
#' @rdname artifacts
fix_artifacts <- function(ts, samp_freq, lim = NULL, baseline = NULL,
                          artifacts = NULL, ..., min_cont = 0.2,
                          max_gap = 1, max_loss = 0.5){
  # store the number of observations in the time series as 'n'
  n <- length(ts)
  # if no logical vector of artifacts has been passed, run get_artifacts
  if(is.null(artifacts)){
    artifacts <- get_artifacts(ts, samp_freq, ...)
  }
  # if the proportion of artifacts exceeds the max_loss threshold, set all
  # values of the time series to missing and return
  if(sum(artifacts) / length(artifacts) > max_loss){
    ts[] <- NA_real_
    return(ts)
  }
  # if limits have been passed, run get_oor and label out-of-range samples
  # as artifacts
  if(!is.null(lim)){
    artifacts <- artifacts |
      get_oor(ts, samp_freq, lim, ..., min_cont = min_cont,
              baseline = baseline, artifacts = artifacts)
    artifacts <- artifacts | is.na(artifacts)
    # if the proportion of artifacts now exceeds the max_loss threshold, set all
    # values of the time series to missing and return
    if(sum(artifacts) / length(artifacts) > max_loss){
      ts[] <- NA_real_
      return(ts)
    }
  }
  # set observations flagged as artifacts to missing
  ts[artifacts] <- NA_real_
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
    h <- gap[length(gap)]
    # [n-2h : n-h] = the interval immediately preceding the final gap that is
    # equal in length to the final gap plus 1.
    # if the length of the final gap is more than half the length of the entire
    # time series, or if there are any missing values in the matched interval,
    # cancel the interpolation.
    if(h > n/2 || any(is.na(ts[(n-2*h):(n-h)]))){
      ts[] <- NA_real_
      return(ts)
    }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(ts[(n-2*h):(n-h)])
    # and repeat these differences over the missing interval
    for(i in 1:h){
      ts[n-h+i] <- ts[n-h+i-1] + lag1[i]
    }
  }
  # if there is missing data at start of trial
  if(is.na(ts[1])){
    # h = the length of the initial gap in the trial
    h = gap[1]
    # [h+1 : 2h+1] = the interval immediately following the first gap that is
    # equal in length to the first gap plus 1
    # if the length of the initial gap is more than half the length of the
    # entire time series, or if there are any missing values in the matched
    # interval, cancel the interpolation.
    if(h > n/2 || any(is.na(ts[(h+1):(2*h+1)]))){
      ts[] <- NA_real_
      return(ts)
    }
    # otherwise, compute the lag-1 differences over the matched interval
    lag1 <- diff(ts[(h+1):(2*h+1)])
    # and repeat these differences over the missing interval
    for(i in h:1){
      ts[i] <- ts[i+1] - lag1[i]
    }
  }
  # threshold to lim argument, if it exists; otherwise to lower bound of 0
  min_lim <- 0; max_lim <- Inf
  if(!is.null(lim)){
    if(is.null(baseline)) baseline <- rep(TRUE, length(ts))
    min_lim <- median(ts[baseline]) * (1+lim[1])
    max_lim <- median(ts[baseline]) * (1+lim[2])
  }
  ts[ts < min_lim] <- min_lim
  ts[ts > max_lim] <- max_lim
  ts
}
