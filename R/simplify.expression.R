simplify.expression <- function(P.num, P.den) {
  if (is.null(P.den)) {
    if (P.num$fraction) {
      P.new <- simplify.expression(P.num$num, P.num$den)
      P.new$sumset <- P.num$sumset
      return(P.new)
    }
    P <- P.num
    if (P$product) {
      parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction | x$sum))
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
  } else {
    P.num <- simplify.expression(P.num, NULL)
    P.den <- simplify.expression(P.den, NULL)
    if (length(P.den$sumset) > 0) {
      P.new <- probability(fraction = TRUE)
      P.new$num <- P.num
      P.new$den <- P.den
      return(P.new)
    } else {
      if (P.num$product) {
        parse.children.num <- sapply(P.num$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction))
        if (sum(parse.children.num) > 0) {
          P.new <- probability(fraction = TRUE)
          P.new$num <- P.num
          P.new$den <- P.den
          return(P.new)
        }
        if (P.den$product) {
          parse.children.den <- sapply(P.den$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction))
          if (sum(parse.children.den) > 0) {
            P.new <- probability(fraction = TRUE)
            P.new$num <- P.num
            P.new$den <- P.den
            return(P.new)
          } else {
            while (length(P.den$children) > 0) {
              P.num$children[[1]] <- NULL
              P.den$children[[1]] <- NULL
            }
            return(P.num)
          }
        } else {
          P.num$children[[1]] <- NULL
          return(P.num)
        }
      }
    }
  }
}
