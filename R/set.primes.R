set.primes <- function(vars, new, counter) {
  primed <- intersect(vars, names(counter))
  initial <- setdiff(vars, names(counter))
  if (new) {
    counter[primed] <- counter[primed] + 1
    counter[initial] <- 1
  } else {
    counter[initial] <- 0
  }
  primes <- sapply(counter, function(x) {
    if (x > 1) return (paste0("^{", paste0(rep("\\prime", x - 1), collapse = ""), "}", collapse = ""))
    return ("")
  })
  return (list(counter = counter, super = setNames(primes, names(counter))))
}