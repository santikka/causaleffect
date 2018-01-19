deconstruct <- function(P, P.context, topo) {

  # Fraction in a context
  if (P$fraction) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P$num, P.context$num, topo)
        P.context$den <- deconstruct(P$den, P.context$den, topo)
        return(P.context)
      }
      if (P.context$product) {
        P.temp <- probability(fraction = TRUE)
        P.temp$fraction <- TRUE
        P.temp$sumset <- P.context$sumset
        P.context$sumset <- character(0)
        P.temp$num <- deconstruct(P$num, P.context, topo)
        P.temp$den <- deconstruct(P$den, probability(), topo)
        return(P.temp)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    } else {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    }
    P.context$fraction <- TRUE
    P.context$sumset <- P$sumset
    P.context$num <- deconstruct(P$num, probability(), topo)
    P.context$den <- deconstruct(P$den, probability(), topo)
    return(P.context)
  }

  # Product in a context
  if (P$product) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product) {
        for (i in 1:length(P$children)) {
          P.context <- deconstruct(P$children[[i]], P.context, topo)
        }
        return(P.context)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability(), topo)
      }
    } else {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num, topo)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability(), topo)
        return(P.context)
      }
    }
    P.context$product <- TRUE
    P.context$sumset <- P$sumset
    for (i in 1:length(P$children)) {
      P.context <- deconstruct(P$children[[i]], P.context, topo)
    }
    return(P.context)
  }

  # Sum in a context
  if (P$sum) {
    if (P.context$fraction) {
      P.context$num <- deconstruct(P, P.context$num, topo)
      return(P.context)
    }
    if (P.context$product || P.context$sum) {
      P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability(), topo)
      return(P.context)
    }
    P.context$sum <- TRUE
    P.context$sumset <- P$sumset
    for (i in 1:length(P$children)) {
      P.context <- deconstruct(P$children[[i]], P.context, topo)
    }
    return(P.context)
  }

  # Atomic expression in a context
  if (P.context$fraction) {
    P.context$num <- deconstruct(P, P.context$num, topo)
    return(P.context)
  }

  if (P.context$product) {
    init <- length(P.context$children)
    if (length(P$sumset) == 0) {
      n <- length(P$var)
      for (i in n:1) {
        P.context$children[[init+n-i+1]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
          domain = P$domain, do = P$do)
      }
    } else {
      n <- length(P$var)
      if (n > 1) {
        P.temp <- probability(product = TRUE, children = list(), sumset = P$sumset)
        for (i in n:1) {
          P.temp$children[[n-i+1]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
            domain = P$domain, do = P$do)
        }
        P.context$children[[init + 1]] <- P.temp
      }
    }
    return(P.context)
  }

  if (P.context$sum) {
    init <- length(P.context$children)
    P.temp <- probability(product = TRUE, children = list(), sumset = P$sumset)
    n <- length(P$var)
    for (i in n:1) {
      P.temp$children[[n-i+1]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
        domain = P$domain, do = P$do)
    }
    P.context$children[[init + 1]] <- P.temp
    return(P.context)
  }

  # Atomic expression with multiple variables
  n <- length(P$var)
  if (n > 1) {
    P.context$product <- TRUE
    P.context$sumset <- P$sumset
    P.context$children <- list()
    for (i in n:1) {
      P.context$children[[n-i+1]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(i:n)]) %ts% topo,
        domain = P$domain, do = P$do)
    }
    return(P.context)
  }

  return(P)
}