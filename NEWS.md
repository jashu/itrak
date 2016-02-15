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
