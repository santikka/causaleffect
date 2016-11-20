simplify.trmz <- function(P.num, P.den, to) {
  if (is.null(P.den)) {
    if (P.num$fraction) {
      P.num$fraction <- FALSE
      P.den <- P.num$divisor
      P.num$divisor <- NULL
      return(simplify.trmz(P.num, P.den, to))
    }
    P <- P.num
    if (P$recursive) {
      parse.children <- sapply(P$children, FUN = function(x) (x$recursive | length(x$sumset) > 0 | x$fraction))
      if (sum(parse.children) > 0) return(P)
      while (length(P$sumset) > 0) {
        last <- length(P$children)
        if (P$children[[last]]$var %in% P$sumset) {
          P$sumset <- setdiff(P$sumset, P$children[[last]]$var)
          P$children[[last]] <- NULL
        } else break
      }
      if (length(P$children) == 1) {
        ch <- P$children[[1]]
        return(probability(var = ch$var, cond = ch$cond, sumset = P$sumset,
          domain = ch$domain, do = ch$do))
      } else return(P)
    } else {
      return(probability(var = setdiff(P$var, P$sumset), cond = P$cond, sumset = c(),
        domain = P$domain, do = P$do))
    }
  } else
  P.num <- simplify.trmz(P.num, NULL, to)
  P.den <- simplify.trmz(P.den, NULL, to)
  if (length(P.den$sumset) > 0) {
    P.num$fraction <- TRUE
    P.num$divisor <- P.den
    return(P.num)
  } else {
    if (P.num$recursive) {
      parse.children.num <- sapply(P.num$children, FUN = function(x) (x$recursive | length(x$sumset) > 0 | x$fraction))
      parse.children.den <- sapply(P.den$children, FUN = function(x) (x$recursive | length(x$sumset) > 0 | x$fraction))
      if (sum(parse.children.num) > 0 | sum(parse.children.den) > 0) {
        P.num$fraction <- TRUE
        P.num$divisor <- P.den
        return(P.num)
      } else {
        while (length(P.den$children) > 0) {
          P.num$children[[1]] <- NULL
          P.den$children[[1]] <- NULL
        }
        return(P.num)
      }
    }
  }
}
