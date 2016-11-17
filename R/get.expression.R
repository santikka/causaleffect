get.expression <- function(x, primes = FALSE) {
  query <- attr(x, "query")
  prime.counter <- setNames(rep(1, length(query)), unlist(query))
  return (get.expression.internal(x, primes, prime.counter, FALSE))
}
