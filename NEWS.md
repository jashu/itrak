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
