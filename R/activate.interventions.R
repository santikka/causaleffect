activate.interventions <- function(P, domain, active) {
  if (P$fraction) {
    P$num <- activate.interventions(P$num, domain, active)
    P$den <- activate.interventions(P$den, domain, active)
    return(P)
  }
  if (P$product) {
    const <- c()
    for (i in 1:length(P$children)) {
      P$children[[i]] <- activate.interventions(P$children[[i]], domain, active)
      if (!P$children[[i]]$product & !P$children[[i]]$fraction) {
        if (P$children[[i]]$var %in% active) {
          const <- c(const, i)
        }
      }
    } 
    if (length(const) > 0) {
      P$children <- P$children[-const]
      if (length(P$children) == 1) {
        ss <- P$sumset
        P <- P$children[[1]]
        P$sumset <- union(P$sumset, ss)
      }
    }
    return(P)
  }
  P$domain <- domain
  P$cond <- setdiff(P$cond, active)
  P$do <- active
  return(P)
}
