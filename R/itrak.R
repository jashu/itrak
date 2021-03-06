#' itrak: Tools for Processing Eye-Tracking Data
#'
#' \code{itrak} provides functions to facilitate the processing of data from
#' visual attention experiments. The functions can be divided into two main
#' categories: attention bias and pupillometry. In addition, more generic
#' functions are provided to address common data-cleaning chores associated
#' with multi-trial experimental data.
#'
#' @section Attention bias:
#' The attention bias functions calculate attention bias scores using data from
#' tasks such as the dot probe. The main function, \code{\link{summarize_bias}},
#' provides a summary table with metrics of mean bias and trial-level bias.
#'
#' @section Pupillometry:
#' The pupillometry functions denoise and standardize data from the
#' continuous measurement of pupil diameter. \code{\link{get_artifacts}} and
#' \code{\link{fix_artifacts}} identify and remove artifacts, such as those
#' caused by blinking, and a \code{\link{low_pass_filter}} is provided to remove
#' other sources of high-frequency noise. \code{\link{normalize}} standardizes
#' pupil measurements into units of relative change from baseline.
#'
#' @section Vignettes:
#' To learn more about \code{itrak}, start with the vignettes:
#' \code{browseVignettes(package = "itrak")}. Note that if you have installed
#' this package using \code{\link[devtools]{install_github}}, you need to add
#' \code{build_vignettes = TRUE} to \code{devtools::install_github} to be able to
#' access the vignettes in this way.
#'
#' @docType package
#' @name itrak

NULL
