compute.q <- 
function(c.comp, v, P) {
  v.len <- length(v)
  product.list <- list()
  P.prod <- NULL
  ind <- which(v %in% c.comp)
  add <- 1
  for (i in 1:length(c.comp)) {
    sum.new <- v[ind[i]:v.len]
    P.prod <- P
    P.prod$sumset <- union(P$sumset, setdiff(sum.new, v[ind[i]]))
    if (P$product | P$fraction) {
      P.num <- P.prod
      P.den <- P.prod
      P.den$sumset <- union(P$sumset, sum.new)  
      P.prod <- simplify.rc(P.num, P.den)
      #P.prod$fraction <- TRUE
      #P.prod$divisor <- P
      #P.prod$divisor$sumset <- union(P$sumset, sum.new)   
    } 
    else {
      P.prod$var <- setdiff(P.prod$var, P.prod$sumset)
      P.prod$sumset <- c()
      if (!identical(P.prod$var, sum.new)) {
        P.prod$cond <- union(P.prod$cond, setdiff(P.prod$var, v[ind[i]]))
        P.prod$var <- v[ind[i]]
      }
    }
    product.list[[i]] <- P.prod  
  }
  if (length(product.list) > 1) {
    product.list <- cancel.rc(product.list)
  	return(probability(product = TRUE, children = product.list))
  } else {
  	return(P.prod)
  }    
}