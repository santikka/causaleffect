get.expression <- function(x, primes = FALSE) {
  return (get.expression.internal(x, primes, c(), FALSE))
}
