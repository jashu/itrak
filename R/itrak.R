#' itrak: Tools for processing eye-tracking data
#'
#' \code{itrak} provides functions to facilitate the processing of data from
#' visual attention experiments. Currently the functions can be divided into two
#' categories: attention bias and pupillometry.
#'
#' @section Attention bias functions:
#' The attention bias functions calculate attention bias scores using data from
#' tasks such as the dot probe. The main function, \code{\link{summarize_bias}},
#' provides a summary table with metrics of mean bias and trial-level bias.
#'
#' @section Pupillometry functions:
#' The pupillometry functions denoise and standardize data from the
#' continuous measurement of pupil diameter. \code{\link{get_artifacts}} and
#' \code{\link{fix_artifacts}} identify and remove artifacts, such as those
#' caused by blinking, and a \code{\link{low_pass_filter}} is provided to remove
#' other sources of high-frequency noise. \code{\link{normalize}} standardizes
#' pupil measurements into units of relative change from baseline.
#'
#' @docType package
#' @name itrak

NULL
