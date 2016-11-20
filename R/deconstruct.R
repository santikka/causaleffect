deconstruct <- function(P, P.context) {

  # Fraction in a context
  if (P$fraction) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P$num, P.context$num)
        P.context$den <- deconstruct(P$den, P.context$den)
        return(P.context)
      }
      if (P.context$product) {
        P.temp <- probability(fraction = TRUE)
        P.temp$fraction <- TRUE
        P.temp$sumset <- P.context$sumset
        P.context$sumset <- character(0)
        P.temp$num <- deconstruct(P$num, P.context)
        P.temp$den <- deconstruct(P$den, probability())
        return(P.temp)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability())
        return(P.context)
      }
    } else {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability())
        return(P.context)
      }
    }
    P.context$fraction <- TRUE
    P.context$sumset <- P$sumset
    P.context$num <- deconstruct(P$num, probability())
    P.context$den <- deconstruct(P$den, probability())
    return(P.context)
  }

  # Product in a context
  if (P$product) {
    if (length(P$sumset) == 0) {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num)
        return(P.context)
      }
      if (P.context$product) {
        for (i in 1:length(P$children)) {
          P.context <- deconstruct(P$children[[i]], P.context)
        }
        return(P.context)
      }
      if (P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability())
      }
    } else {
      if (P.context$fraction) {
        P.context$num <- deconstruct(P, P.context$num)
        return(P.context)
      }
      if (P.context$product || P.context$sum) {
        P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability())
        return(P.context)
      }
    }
    P.context$product <- TRUE
    P.context$sumset <- P$sumset
    for (i in 1:length(P$children)) {
      P.context <- deconstruct(P$children[[i]], P.context)
    }
    return(P.context)
  }

  # Sum in a context
  if (P$sum) {
    if (P.context$fraction) {
      P.context$num <- deconstruct(P, P.context$num)
      return(P.context)
    }
    if (P.context$product || P.context$sum) {
      P.context$children[[length(P.context$children)+1]] <- deconstruct(P, probability())
      return(P.context)
    }
    P.context$sum <- TRUE
    P.context$sumset <- P$sumset
    for (i in 1:length(P$children)) {
      P.context <- deconstruct(P$children[[i]], P.context)
    }
    return(P.context)
  }

  # Atomic expression in a context
  if (P.context$fraction) {
    P.context$num <- deconstruct(P, P.context$num)
    return(P.context)
  }

  if (P.context$product) {
    init <- length(P.context$children) 
    if (length(P$sumset) == 0) {
      P.temp <- probability(var = P$var[1], cond = union(P$cond, P$var[-1]),
        domain = P$domain, do = P$do)
      P.context$children[[init + 1]] <- P.temp
      if (length(P$var) > 1) {
        for (i in 2:length(P$var)) {
          P.temp <- probability(var = P$var[i], cond = union(P$cond, P$var[-(1:i)]),
            domain = P$domain, do = P$do)
          P.context$children[[init + i]] <- P.temp
        }
      }
    } else {
      if (length(P$var) > 1) {
        P.temp <- probability(product = TRUE, children = list(), sumset = P$sumset) 
        P.temp$children[[1]] <- probability(var = P$var[1], cond = union(P$cond, P$var[-1]),
          domain = P$domain, do = P$do)
        for (i in 2:length(P$var)) {
          P.temp$children[[i]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(1:i)]),
            domain = P$domain, do = P$do)
        }
        P.context$children[[init + 1]] <- P.temp
      }
    }
    return(P.context)
  }

  if (P.context$sum) {
    P.temp <- probability(product = TRUE, children = list(), sumset = P$sumset) 
    P.temp$children[[1]] <- probability(var = P$var[1], cond = union(P$cond, P$var[-1]),
      domain = P$domain, do = P$do)
    if (length(P$var) > 1) {
      for (i in 2:length(P$var)) {
        P.temp$children[[i]] <- probability(var = P$var[i], cond = union(P$cond, P$var[-(1:i)]),
          domain = P$domain, do = P$do)
      }
    }
    P.context$children[[init + 1]] <- P.temp
    return(P.context)
  }

  return(P)
}