cancel.rc <- function(productlist) {
  nums <- list()
  dens <- list()
  for (i in 1:length(productlist)) {
    if (productlist[[i]]$fraction) {
      dens[[i]] <- productlist[[i]]$divisor
    } else dens[[i]] <- 0
    nums[[i]] <- productlist[[i]]
  }
  i <- 1
  j <- 1
  remove <- c()
  while (i <= length(dens)) {
    if (!identical(dens[[i]], 0)) {
      while (j <= length(nums)) {
        if (identical(dens[[i]], probability(product = TRUE, sumset = nums[[j]]$sumset, children = nums[[j]]$children)) && !(j %in% remove)) {
          nums[[i]]$fraction <- FALSE
          nums[[i]]$divisor <- NULL
          remove <- c(remove, j)
          j <- 1
          break
        }
        j <- j + 1
  	  }
    }
    i <- i + 1
  }
  nums[remove] <- NULL
  return (nums)
}