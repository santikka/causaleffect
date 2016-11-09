simplify.rc <-
function(P.num, P.den) {
  if (is.null(P.den)) {
    if (P.num$fraction) {
      P.new <- simplify.rc(P.num$num, P.num$den)
      P.new$sumset <- P.num$sumset
      return(P.new)
    }
    P <- P.num
    if (P$product) {
      parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction))
      if (sum(parse.children) > 0) return(P)
      remove.sum <- c()
      remove.ch <- c()
      i <- length(P$sumset)
      for(j in length(P$children):1) {
        if (P$children[[j]]$var == P$sumset[i]) {
          remove.sum <- c(remove.sum, P$sumset[i])
          remove.ch <- c(remove.ch, i)
          i = i - 1
          if (i == 0) break
        } else {
          if (P$sumset[i] %in% P$children[[j]]$cond) break
        }
      }
      P$sumset <- setdiff(P$sumset, remove.sum)
      if (length(remove.ch) > 0) P$children[remove.ch] <- NULL
      if (length(P$children) == 0) return(NULL)
      if (length(P$children) == 1) {
        ch <- P$children[[1]] 
        return(probability(var = ch$var, cond = ch$cond, sumset = P$sumset))
      } 
    } 
    return(P)
  } else {
    P.num <- simplify.rc(P.num, NULL)
    P.den <- simplify.rc(P.den, NULL)
    if (is.null(P.den)) return(P.num)
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
            while(length(P.den$children) > 0) {
              P.num$children[[1]] <- NULL
              P.den$children[[1]] <- NULL     
            }
            return(P.num)
          }
        } else {
          P.num$children[[1]] <- NULL
          return(P.num)
        }
      } else {
        P.new <- probability(fraction = TRUE)
        P.new$num <- P.num
        P.new$den <- P.den
        return(P.new) 
      }
    }
  }
}
