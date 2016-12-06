#' @export
get_gaze_bias <- function(gaze, left, neutral = "Neutral"){
  if(length(gaze) != 2) stop("gaze must be a vector of length 2")
  if(any(is.na(gaze)) || any(is.na(left))) return(NA)
  if(left[1] == neutral){
    neu <- gaze[1]
    emo <- gaze[2]
  } else {
    neu <- gaze[2]
    emo <- gaze[1]
  }
  emo - neu
}
