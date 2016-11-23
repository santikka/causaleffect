compute.c.factor <- function(cc, v, P, to) {
  v.len <- length(v)
  cc.len <- length(cc)
  product.list <- list()
  ind <- which(v %in% cc)
  add <- 1
  for (i in cc.len:1) {
    sum.new <- v[ind[i]:v.len]
    P.prod <- P
    P.prod$sumset <- union(P$sumset, setdiff(sum.new, v[ind[i]]))
    if (P$product | P$fraction) {
      P.num <- P.prod
      P.den <- P.prod
      P.den$sumset <- union(P$sumset, sum.new) %ts% to
      P.prod <- probability(fraction = TRUE, num = P.num, den = P.den)
    }
    else {
      P.prod$var <- setdiff(P.prod$var, P.prod$sumset)
      P.prod$sumset <- character(0)
      if (!identical(P.prod$var, sum.new)) {
        P.prod$cond <- union(P.prod$cond, setdiff(P.prod$var, v[ind[i]])) %ts% to
        P.prod$var <- v[ind[i]]
      }
    }
    product.list[[cc.len - i + 1]] <- P.prod
  }
  if (length(product.list) > 1) {
    return(cancel.rc(product.list))
  } else {
    return(product.list[[1]])
  }
}