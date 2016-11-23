## eliminate "data islands" between artifacts (periods less than min_cont)
merge_artifacts <- function(artifacts, margin){
  runs <- rle(artifacts)
  island <- which(runs$lengths < margin & !runs$values)
  if(length(island) > 0){
    for (i in seq_along(island)){
      start_index <- sum(runs$lengths[0:(island[i] - 1)]) + 1
      end_index <- sum(runs$lengths[1:island[i]])
      artifacts[start_index:end_index] <- TRUE
    }
  }
  artifacts
}

## expand artifact to include margin (period equal to min_cont)
buffer_artifacts <- function(artifacts, margin){
  runs <- rle(artifacts)
  n_runs <- length(runs$values)
  if(n_runs < 2) return(artifacts)
  pivots <- cumsum(runs$lengths)
  for(i in 1:(n_runs-1)){
    period <- artifacts[pivots[i]:pivots[i+1]]
    if(runs$values[i]){
      if(i == 1) period <-artifacts[1:pivots[i]] else next
    }
    # find center of artifact
    dist_from_left <- median(which(period == min(period)))
    dist_from_right <- length(period) - dist_from_left
    center <- pivots[i] + dist_from_left
    if(runs$values[i]) center <- dist_from_left
    # expand artifact to be symmetrical about center
    dist_from_center <- dist_from_left
    if(dist_from_right > dist_from_left) dist_from_center <- dist_from_right
    onset <- center - dist_from_center - margin/2
    if(onset < 1) onset <- 1
    offset <- center + dist_from_center + margin/2
    if(offset > length(artifacts)) offset <- length(artifacts)
    artifacts[onset:offset] <- TRUE
  }
  artifacts
}
