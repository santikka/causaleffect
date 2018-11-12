cancel.rc <- function(product.list) {
  nums <- list()
  dens <- list()
  for (i in 1:length(product.list)) {
    if (product.list[[i]]$fraction) {
      dens[[i]] <- product.list[[i]]$den
      nums[[i]] <- product.list[[i]]$num
    } else {
      nums[[i]] <- product.list[[i]]
    }
  }
  i <- 1
  j <- 1
  remove.num <- c()
  remove.den <- c()
  while (i <= length(dens)) {
    while (j <= length(nums)) {
      if (identical(dens[[i]], nums[[j]]) && !(j %in% remove.num)) {
        remove.num <- c(remove.num, j)
        remove.den <- c(remove.den, i)
        j <- 1
        break
      }
      j <- j + 1
    }
    i <- i + 1
  }
  nums[remove.num] <- NULL
  dens[remove.den] <- NULL
  if (length(dens) == 0) {
    if (length(nums) == 1) return(nums[[1]])
    return (probability(product = TRUE, children = nums))
  }
  return(probability(fraction = TRUE,
    num = probability(product = TRUE, children = nums),
    den = probability(product = TRUE, children = dens)
    )
  )
}