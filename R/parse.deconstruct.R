parse.deconstruct <-
function(P) {
  if (P$product) {
    for (i in 1:length(P$children)) {
      P$children[[i]] <- parse.deconstruct(P$children[[i]])
    }
  } 
  if (P$fraction) {
    i <- 1
    k <- 0
    if (length(P$num$sumset) == 0) {
      while (i <= length(P$num$children) & length(P$num$children) > 0 & length(P$den$children) > 0) {
        is.element <- FALSE
        for (j in 1:length(P$den$children)) {
          if (identical(P$num$children[[i]], P$den$children[[j]], attrib.as.set = TRUE)) {
            is.element <- TRUE
            k <- j
            break
          }
        }
        if (is.element) {
          P$num$children[i] <- NULL
          P$den$children[k] <- NULL
          i <- 0
        }
        i <- i + 1
      }
    }
    if (length(P$den$children) == 0) {
      return(P$num)
    }
  }
  return(P)
}
