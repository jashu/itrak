# itrak 0.0.0.9201

This update reflects a major overhaul in the implementation of the `get_artifacts` and `fix_artifacts` functions.

## Improved speed

The fundamental algorithm for identifying artifacts remains essentially unchanged, but the underlying code now implements a vectorized (as opposed to iterative) execution, resulting in more than a 10X speedup in the runtime of `get_artifacts` without any loss in performance.

## Improved performance

Occasionally, for especially noisy time series, the previous version of `get_artifacts` could miss some of the less severe artifacts. These would have to be spotted using `plot_artifacts` and manually identified before running `fix_artifacts`, or, more expediently, one could take the output of `fix_artifacts` (after the more severe artifacts have been removed) and run `get_artifacts` again to identify remaining artifacts. This latter recursive solution is now implemented automatically so that the output of `get_artifacts` reflects the ultimate conclusion of a back-and-forth between `get_artifacts` and `fix_artifacts` until `get_artifacts` cannot identify any artifacts in the output of `fix_artifacts`.

## Separation of signal-spike artifacts from signal-drift artifacts

Previously, the `get_artifacts` function would apply `lim`, `baseline`, `max_loss`, and `max_gap` arguments to the identification of artifacts. Periods of signal-spike artifacts (e.g. blinks) would be identified in the same step as periods of signal-drift artifacts (i.e., values outside the range specified in the `lim` argument). And if `max_loss` or `max_gap` criteria were violated, the entire time series would be marked as artifacts. This conflation made it difficult to evaluate how well signal spikes are being labeled in such trials when using `plot_artifacts`. The new implementation of `get_artifacts` only labels signal-spike artifacts. Signal-drift artifacts can now be obtained separately via the `get_oor` ("oor" stands for "out of range") function, and the `plot_artifacts` now includes an optional `oor` argument. As before, signal-spike artifacts will be indicated by red dotted lines, but now out-of-range artifacts will be indicated by blue dotted lines if an `oor` vector is passed. The `max_loss` and `max_gap` criteria for trial exclusion are no longer implemented at the `get_artifacts` stage, but rather at the `fix_artifacts` stage.

## `lim` argument evaluated AFTER interpolation/extrapolation over artifacts

Previously, the `lim` argument was evaluated prior to artifact removal by `fix_artifacts`. This worked well in most cases, but occasionally led to erroneous identification of signal-drift artifacts when the period identified as `baseline` consisted primarily of signal-spike artifacts. In the new implementation, signal-drift (out-of-range) artifacts are evaluated *after* spike-artifact correction, insuring that the baseline reference period contains only clean data.

## `max_velocity` now specified in quantile units instead of absolute units

Previously, `get_artifacts` permitted the user to override the automatic determination of the magnitude of change between samples that distinguishes normal signal change from the onset (or offset) of an artifact. This was occasionally necessary when dealing with unusually erratic time series. The new recursive implementation of `get_artifacts` appears to take care of such scenarios, obviating the need for this argument. Instead, the user is now permitted to use this argument to specify a different quantile for determining velocity outliers other than the default `0.9` (90th percentile). Extensive testing established `0.9` as the optimal cutoff for pupil change, and there should be no need to change this setting if you are applying `get_artifacts` to pupillometry data. However, if you are experimenting with other kinds of time series, another quantile may prove more optimal, and you may wish to increase the `max_velocity` quantile to reduce false positives, or decrease it to reduce false negatives.

## List of specific changes to user interface

The above changes are reflected by the following changes to the function arguments:

1. `get_artifacts` no longer accepts the `baseline`, `lim`, `max_loss`, and `max_gap` arguments. It is now streamlined to 2 required arguments (`ts` and `samp_freq`) and 2 optional arguments (`min_cont` and `max_velocity`).

2. The `max_velocity` argument used by `get_artifacts` is now specified in quantile units.

3. The `threshold` argument has been removed from `fix_artifacts`. This argument was only rarely relevant. It only applies when the time series either begins or ends with an artifact and the extrapolation performed by `fix_artifacts` exceeds the range specified by `lim`. The user could formerly set `threshold = FALSE`, which would result in the complete rejection of such trials. Thresholding is now always performed when extrapolating. There is no reason to reject trials that have been thresholded so long as the period of thresholding is short, and trials will still be rejected if the thresholded period exceeds the `max_gap` argument.

4. New `get_oor` function for explicitly identifying data points that are out of range. Documented in the same help file with `get_artifacts` and `fix_artifacts`.

5. `plot_artifacts` adds an `oor` argument for labeling out-of-range data distinct from artifact data.


# itrak 0.0.0.9110

This update reflects a minor change to the artifact detection defaults and a
major revision to the `get_tlbs` function.

## New default `lim` argument

Based on user feedback, the default setting for the `min_cont` argument in `get_artifacts` has been changed from 100 ms to 200 ms. This appears to give better artifact identification over a wider range of cases without introducing false positives in pupil diameter time series. But as always you should scrutinize artifact-identification plots and adjust default parameters if that is appropriate for your data.

## Revisions to `get_tlbs`, `get_bs`, and `summarize_bias` functions

1. `get_tlbs` now includes a new method I developed for calculating trial-level bias scores (TL-BS). The old method used by Zvielli et al. (2015) is still available by setting the new parameter `weighted` to `FALSE`. For the first and last 2 pairs of an incongruent trial (IT) and congruent trial (CT), the methods are equivalent, but for all trials in between the weighted method calculates the mean of the preceding and subsequent trials of opposite type, with the closer trial weighted more heavily than the more distant trial. This is computed by constructing a CT time series with missing values in place of ITs (and vice versa), performing a linear interpolation over the missing values, and then subtracting the approximated complete CT time series from the approximated complete IT time series. Thus, each real CT is subtracted from a projected IT that lies on an imaginary line connecting the two surrounding real ITs, and a similarly projected CT is subtracted from each real IT. 

    The two methods yield highly similar TL-BS numbers, but the weighted method may be preferable for two reasons: 1) In the event that a trial of one type is equidistant from two trials of opposite type, the Zvielli method arbitrarily chooses one over the other; under this circumstance, the mean of the two trials may be a more valid point of comparison. 2) The Zvielli method frequently double-counts the same IT-CT subtraction. For example, consider the sequence IT IT CT CT: under the Zvielli method, the interior IT-CT pair will result in duplicate TL-BS calculations for these two trials. This double counting results in brief but frequent artifactual periods where the TL-BS time series is completely flat. (See examples in the help file.) Under the weighted method, these calculations will be non-identical because a trial is not subtracted directly from another single trial, but rather from the weighted mean of unique trial pairs. In the above example, the interior CT would not be subtracted from the preceding IT, but rather the weighted average of the preceding IT and whatever IT comes next.

2. Arguments for all of the attention-bias functions have been renamed and simplified as follows:

    a. The `measure` argument has been renamed to `RT` to make it more obvious which argument corresponds to reaction times. Functionally, however, it works just the same.
    
    b. The `type` and `reference` arguments have been replaced by a single `congruent` argument, which should be a logical vector indicating whether a given trial is congruent (`TRUE`) or incongruent (`FALSE`). This is hopefully easier to use and less confusing, and makes the function code much easier to read for anyone wishing to understand how the function works.
    
    c. There is a new `weighted` argument that controls whether the nearest-trial method of Zvielli et al. will be used (`weighted = FALSE`) vs. the weighted-trials method discussed above (`weighted = TRUE`). By default, the function will use the new weighted method.
    
    d. The `search_limit` argument remains unchanged.
 
**IMPORTANT:** Note that if you have previously written code with any of the above functions, this update will break them. You will need to create a logical test for congruent trials to pass to the `congruent` parameter, remove any `type` or `reference` arguments, and optionally add a `weighted = FALSE` argument if you wish to keep using the original Zvielli et al. method.

# itrak 0.0.0.9100

This update reflects a major overhaul of the software architecture of the pupillometry functions to improve usability and performance, and adds several new features to help prepare your data for artifact cleaning:

## No More Default `lim` Argument

`get_artifacts` and `fix_artifacts` now require users to specify their own `lim` argument, which sets limits on the permissible amount of relative change from baseline. The previous default argument `lim = c(-0.5, 0.5)` was based on measurements of pupil *diameter*. For users measuring pupil *area*, these limits will likely prove too conservative, especially the upper limit. Optimal `lim` settings will also likely depend on equipment, lighting conditions, and experimental design. While `get_artifacts` and `fix_artifacts` perform reasonably well without *any* bounds on relative change (an overly conservative `lim` will generate far more false positives than an overly liberal `lim` will generate false negatives), prior knowledge about the range of possible values for your experiment can and should be used to optimize artifact detection. For this reason, the default setting has been removed, and a section with advice on how to set the `lim` argument has been added to the documentation.

## Greater Cohesion and Less Redundancy Across Functions

1. Due to the extensive functional overlap between `get_artifacts` and `fix_artifacts`, these functions are now documented together in the same help file.

2. Previously, thresholding of extrapolations to the range specified in `lim`
was performed by `fix_artifacts`, and users were not given the option to reject trials with extrapolations at this step. Rather, they were given this option in the subsequent `normalize` step. Thresholding is a more logical choice to make in the context of cleaning artifacts than normalizing them, so this functionality has been added to `fix_artifacts` and removed from `normalize`, which no longer accepts the `threshold` or `lim` argument. Not only is the `lim` argument a redundant check for `normalize` to perform (since `get_artifacts` and `fix_artifacts` have already dealt with this issue), it arguably does not belong in a function called `normalize`, which should just do one thing--rescale the time series to baseline--and nothing more.

## New Features

This update introduces several new functions to assist with pupil data pre-preprocessing (getting your data set up prior to using `get_artifacts` and its antecedents)

1. `get_freq`: If you are unsure about the sampling frequency of your time series, `get_freq` can verify this, provided your data contains a time stamp for each measurement.

2. `label_trial`: If your time series vector contains a larger time series that represents a sequence of smaller time series (as would occur in a multi-trial experiment), it is often useful to explicitly represent this meta-sequence in statistical models (e.g., as trial effects). If your time series data do not already represent trials explicitly, `label_trial` can be used to construct this variable. 

3. `merge_eyes`: Aggregates parallel time series from the left and right eyes into a single time series of measurements. For binocular readings, this will return the mean time series for both eyes; for monocular readings, the time series for the eye that was tracked.

4. `zero_onset`: Centers the sample index or time stamp of your time series to a reference marker, such that 0 always coincides with the onset of a trial.

5. `clip_trials`: Clips all trials to a standard length with the same start and stop times, and issues a warning if there are any missing samples.

## Minor Improvements

1. Additional defensive code has been added to check arguments and issue informative error messages across several functions.

2. More extensive documentation has been added to several functions.

3. `get_artifacts` and `fix_artifacts` now use the median, rather than the mean, of the baseline period as a reference for evaluating limits placed on relative change. Setting a baseline reference helps to catch additional artifacts by using prior knowledge about plausible change from baseline. However, there is a catch-22 if the baseline period itself contains artifacts. The median will be less influenced by the presence of artifacts than the mean and thus should provide a better reference value for evaluating the `lim` argument.

4. Automatic scaling for `plot_artifacts` has been improved. Note that you can always override automatic scaling or any other plotting aesthetic by appending a `ggplot` layer. This is now explained in `plot_artifacts`'s documentation. 


# itrak 0.0.0.9004

## Bug Fixes

1. Fixes typo in the default `lim` argument for `fix_artifacts`.

2. `fix_artifacts` would fail in the previous version if you attempted to pass your own baseline argument in conjunction with `dplyr`'s `group_by` and 
`mutate` (but would succeed with a direct call). For some unknown reason, if `fix_artifacts` performs logical indexing with `!is.na(ts) & baseline`, it fails with `dplyr`, but, if it first converts missing values to `0`s and then indexes with `ts > 0 & baseline`, it succeeds. 

## Improvements

1. `get_artifacts` now takes into account sampling freq when automatically determining the `max_velocity` setting (if no `max_velocity` argument is given). As it turns out, a `max_velocity` determination based on the distribution of lag-1 differences was only valid for ~ 60 Hz sampling. With ~ 500 Hz sampling, the distribution of lag-1 differences is too highly skewed toward 0. The new version evaluates velocity over whatever lag comes closest to 20 ms, e.g., lag-10 differences for 500 Hz, resulting in more stable performance over a wider range of sampling frequencies when the default settings are used.

2. `plot_artifacts` now enables multi-trial plotting. See its documentation for the new syntax for calling this function.

## New Features

1. `itrak` now has a `plot_comparison` function for visually comparing two versions (e.g., original vs. cleaned) of the same time series. Use this plotting function to evaluate the performance of the `fix_artifacts`, `normalize`, and/or `low_pass_filter` functions when cleaning your time series. Note that if your original and cleaned time series are at different scales (e.g., if you are comparing a pre-normed version to a post-normed version), the original time series will be projected to the scale of the cleaned time series.
