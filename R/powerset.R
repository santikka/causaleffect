powerset <- function(set) {
  n <- length(set)
  if (n == 0) return(list(c()))
  indices <- sapply(0:(2^n-1), function(p) as.logical(intToBits(p)[1:n]), simplify = FALSE)
  subsets <- lapply(indices, function(i) set[i])
  return(subsets)
}