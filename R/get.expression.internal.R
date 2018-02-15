get.expression.internal <- function(x, primes, prime.counter, start.sum, target.sym, single.source) {
  P <- ""
  s.print <- length(x$sumset) > 0
  super <- character(0)
  sum.string <- character(0)
  var.string <- character(0)
  cond.string <- character(0)
  if (s.print) {
    if (primes) {
      update <- set.primes(x$sumset, TRUE, prime.counter)
      super <- update$super
      prime.counter <- update$counter
      sum.string <- paste0(x$sumset, super[x$sumset], collapse = ",")
    } else {
      sum.string <- paste0(x$sumset, collapse = ",")
    }
    if (start.sum) P <- paste0(P, "\\left(\\sum_{", sum.string, "}", collapse = "")
    else {
      P <- paste0(P, "\\sum_{", sum.string, "}", collapse = "")
    }
  }
  if (x$fraction) {
    P <- paste0(P, "\\frac{", get.expression.internal(x$num, primes, prime.counter, start.sum, target.sym, single.source),
      "}{", get.expression.internal(x$den, primes, prime.counter, start.sum, target.sym, single.source), "}", collapse = "")
  }
  if (x$sum) {
    P <- paste(P, "\\left(", sep = "", collapse = "")
    add.strings <- c()
    n <- length(x$children)
    for (i in 1:length(x$children)) {
      add.strings[i] <- paste0(c("w_{", i, "}^{(", x$weight, ")}", 
        get.expression.internal(x$children[[i]], primes, prime.counter, n > 1, target.sym, single.source)), collapse = "")
    }
    add.strings <- paste(add.strings, sep = "", collapse = " + ") 
    P <- paste0(P, add.strings, "\\right)", collapse = "")
  }
  if (x$product) {
    n <- length(x$children)
    for (i in 1:n) P <- paste0(P,
      get.expression.internal(x$children[[i]], primes, prime.counter, n > 1, target.sym, single.source), collapse = "")
  }
  if (!(x$sum || x$product || x$fraction)) {
    P <- paste0(P, "P", collapse = "")
    if (length(x$do) > 0) {
      do.string <- paste0(x$do, collapse = ",")
      P <- paste0(P, "_{", do.string, "}" , collapse = "")
    }
    if (primes) {
      update <- set.primes(x$var, FALSE, prime.counter)
      super <- update$super
      prime.counter <- update$counter
      var.string <- paste0(x$var, super[x$var], collapse = ",")
    } else {
      var.string <- paste0(x$var, collapse = ",")
    }
    if (x$domain > 0) {
      if (x$domain == 1) P <- paste0(P, target.sym, var.string, collapse = "")
      else {
        if (single.source) P <- paste0(P, "(", var.string, collapse = "")
        else P <- paste0(P, "^{(", x$domain - 1, ")}(", var.string, collapse = "")
      }
    } else {
      P <- paste0(P, "(", var.string, collapse = "")
    }
    if (length(x$cond) > 0) {
      if (primes) {
        update <- set.primes(x$cond, FALSE, prime.counter)
        super <- update$super
        prime.counter <- update$counter
        cond.string <- paste0(x$cond, super[x$cond], collapse = ",")
      } else {
        cond.string <- paste0(x$cond, collapse = ",")
      }
      cond.string <- paste0("\u007C", cond.string, ")", collapse = "")
    }
    else cond.string <- ")"
    P <- paste0(P, cond.string)
  }
  if (s.print & start.sum) P <- paste0(P, "\\right)", collapse = ",")
  return(P)
}