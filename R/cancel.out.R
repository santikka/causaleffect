cancel.out <- function(P) {
  if (P$product) {
    for (i in 1:length(P$children)) {
      P$children[[i]] <- cancel.out(P$children[[i]])
    }
  } 
  if (P$fraction) {
    P$num <- cancel.out(P$num)
    P$den <- cancel.out(P$den)
    i <- 1
    k <- 0
    if (length(P$num$sumset) == 0 && length(P$den$sumset) == 0 && P$num$product && P$den$product) {
      while (i <= length(P$num$children) && length(P$num$children) > 0 && length(P$den$children) > 0) {
        is.element <- FALSE
        for (j in 1:length(P$den$children)) {
          if (identical(P$num$children[[i]], P$den$children[[j]], attrib.as.set = TRUE)) {
            is.element <- TRUE
            k <- j
            break
          }
        }
        if (is.element) {
          P$num$children <- P$num$children[-i]
          P$den$children <- P$den$children[-k]
          i <- 0
        }
        i <- i + 1
      }
    }
    if (length(P$den$children) == 0 && !P$den$fraction) {
      P$num$sumset <- P$sumset
      return(P$num)
    }
    if (length(P$den$children) == 1) {
      P$den$children[[1]]$sumset <- union(P$den$sumset, P$den$children[[1]]$sumset)
      P$den <- P$den$children[[1]]
    }
    if (length(P$num$children) == 1) {
      P$num$children[[1]]$sumset <- union(P$num$sumset, P$num$children[[1]]$sumset)
      P$num <- P$num$children[[1]]
    }
  }
  return(P)
}
