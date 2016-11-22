id <- function(y, x, P, G, to, tree) {
  G.obs <- observed.graph(G)
  v <- get.vertex.attribute(G, "name")
  v <- to[which(to %in% v)]
  anc <- ancestors(y, G.obs, to)
  if (length(P$var) == 0 & !(P$product | P$fraction)) tree$call <- list(y = y, x = x, P = probability(var = v), G = G, line = "", v = v)
  else tree$call <- list(y = y, x = x, P = P, G = G, line = "", v = v)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, y), P$sumset)
      # P <- simplify.expression(P, NULL)
    } else {
      P$var <- y
    }
    tree$call$line <- 1
    tree$root <- P
    return(list(P = P, tree = tree))
  }

  # line 2
  if (length(setdiff(v, anc)) != 0) {
    anc.graph <- induced.subgraph(G, anc)
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, anc), P$sumset)
      # P <- simplify.expression(P, NULL)
    } else {
      P$var <- anc
    }
    nxt <- id(y, intersect(x, anc), P, anc.graph, to, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$anc <- anc
    return(list(P = nxt$P, tree = tree))
  }

  # line 3
  G.x.overbar <- subgraph.edges(G, E(G)[!to(x)], delete.vertices = FALSE)
  anc.xbar <- ancestors(y, observed.graph(G.x.overbar), to)
  w <- setdiff(setdiff(v, x), anc.xbar)
  if (length(w) != 0) {
    r <- blocked(w, y, G.x.overbar, to)
    nxt <- id(y, union(x, setdiff(w, r)), P, induced.subgraph(G, setdiff(v, r)), to, list())
    # nxt <- id(y, union(x, w), P, G, to, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 3
    tree$call$w <- w
    tree$call$anc.xbar <- anc.xbar
    return(list(P = nxt$P, tree = tree))
  }

  # line 4
  G.remove.x <- induced.subgraph(G, v[!(v %in% x)])
  s <- c.components(G.remove.x, to)
  if (length(s) > 1) {
    tree$call$line <- 4
    nxt <- lapply(s, function(t) {
      return(id(t, setdiff(v, t), P, G, to, list()))
    })
    product.list <- lapply(nxt, "[[", "P")
    tree$branch <- lapply(nxt, "[[", "tree")
    return(list(
      P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = product.list),
      tree = tree
    ))
  } else {
    s <- s[[1]]

    # line 5 
    cc <- c.components(G, to)
    if (identical(cc[[1]], v)) {
      v.string <- paste(v, sep = "", collapse = ",")
      s.string <- paste(s, sep = "", collapse = ",")
      tree$call$line <- 5
      stop("Graph contains a hedge formed by C-forests of nodes: \n {", v.string , "} and {", s.string , "}.", call. = FALSE)
    }
   
    # line 6
    pos <- Position(function(x) identical(s, x), cc, nomatch = 0)
    if (pos > 0) {
      tree$call$line <- 6
      tree$call$s <- s
      ind <- which(v %in% s)
      s.len <- length(s)
      product.list <- vector(mode = "list", length = s.len)
      P.prod <- probability()
      for (i in s.len:1) {
        # cond.set <- causal.parents(s[i], v[1:ind[i]], G, G.obs, to)
        cond.set <- v[0:(ind[i]-1)]
        if (P$product) {
          P.prod <- parse.joint(P, s[i], cond.set, v)
          # P.prod <- simplify.expression(P.prod, NULL)
        } else {
          P.prod <- P
          P.prod$var <- s[i]
          P.prod$cond <- cond.set
        }
        product.list[[s.len - i + 1]] <- P.prod
      }
      if (s.len > 1) {
        P.new <- probability(sumset = setdiff(s, y), product = TRUE, children = product.list)
        # P.new <- simplify.expression(P.new, NULL)
        tree$root <- P.new
        return(list(P = P.new, tree = tree))
      } 
      if (P.prod$product | P.prod$fraction) {
        P.prod$sumset <- union(P.prod$sumset, setdiff(s, y))
        # P.prod <- simplify.expression(P.prod, NULL)
      } else {
        P.prod$var <- setdiff(P.prod$var, union(P.prod$sumset, setdiff(s, y)))
      }
      tree$root <- P.prod
      return(list(P = P.prod, tree = tree))
    }

    # line 7
    tree$call$s <- s
    s <- Find(function(x) all(s %in% x), cc)
    tree$call$line <- 7
    tree$call$s.prime <- s
    s.len <- length(s)
    product.list <- vector(mode = "list", length = s.len)
    ind <- which(v %in% s)
    s.graph <- induced.subgraph(G, s)
    for (i in s.len:1) {
      # cond.set <- causal.parents(s[i], v[1:ind[i]], G, G.obs, to)
      cond.set <- v[0:(ind[i]-1)]
      P.prod <- P
      P.prod$var <- s[i]
      P.prod$cond <- cond.set
      product.list[[s.len - i + 1]] <- P.prod
    }
    x.new <- intersect(x, s)
    nxt <- NULL
    if (s.len > 1) nxt <- id(y, x.new, probability(product = TRUE, children = product.list), s.graph, to, list())
    else nxt <- id(y, x.new, product.list[[1]], s.graph, to, list())
    tree$branch[[1]] <- nxt$tree 
    return(list(P = nxt$P, tree = tree))
  }
}
