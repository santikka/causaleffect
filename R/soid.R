soid <- function(y, x, P, G, G.obs, I, J, v, topo, tree) {
  .to <- NULL
  .from <- NULL
  description <- NULL
  if (length(P$var) == 0 & !(P$product | P$fraction)) tree$call <- list(y = y, x = x, P = probability(var = v), G = G, line = "", v = v)
  else tree$call <- list(y = y, x = x, P = P, G = G, line = "", v = v)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, y), P$sumset) %ts% topo
    } else {
      P$var <- y
    }
    tree$call$line <- 1
    tree$root <- P
    return(list(P = P, tree = tree))
  }

  an <- ancestors(y, G.obs, topo)

  # line 2
  if (length(setdiff(v, an)) != 0) {
    G.an <- igraph::induced_subgraph(G, an)
    G.an.obs <- observed.graph(G.an)
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, an), P$sumset) %ts% topo
    } else {
      P$var <- an
    }
    nxt <- soid(y, intersect(x, an), P, G.an, G.an.obs, I, J, an, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$an <- an
    return(list(P = nxt$P, tree = tree))
  }

  if (length(J) == 0) {
    dep <- dependencies(P)
    if (length(I) > 0) {
      for (i in I) {
        ivars <- c(i$y, i$x, i$z)
        if (all(dep %in% ivars) | !(P$product | P$fraction)) {
          if (all(y %in% i$y) & all(i$x %in% x) & all(setdiff(x, i$x) %in% i$y) & (length(i$z) == 0)) {
            G.l <- G
            v.l <- v
            if (!all(dep %in% ivars)) {
              G.l <- latent.projection(G.l, setdiff(v.l, ivars))
              v.l <- ivars
            }
            P.new <- P
            P.new$do <- i$x
            v.l <- v.l[!(v.l %in% i$x)]
            G.lz <- igraph::induced_subgraph(G.l, v.l)
            G.lz.obs <- observed.graph(G.lz)
            nxt <- soid(y, setdiff(x, i$x) %ts% topo, P, G.lz, G.lz.obs, I, i$x, v.l, topo, list())
            nxt$P <- activate.interventions(nxt$P, 0, i$x)
            tree$branch[[1]] <- nxt$tree
            tree$call$line <- 11 #CHANGE
            return(list(P = nxt$P, tree = tree))
          }
          if (setequal(y, c(i$y, i$z)) & setequal(i$x, x)) {
            P.new <- probability(var = i$y, do = i$x, cond = i$z)
            nxt <- soid(i$z, x, P, G, G.obs, I, J, v, topo, list())
            tree$branch[[1]] <- nxt$tree
            tree$call$line <- 11 #CHANGE
            return(list(P = probability(product = TRUE, children = list(P.new, nxt$P))))
          }
        }
      }
    }
  }

  # line 3
  G.xbar <- igraph::subgraph_from_edges(G, igraph::E(G)[!(.to(x) | (.from(x) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
  an.xbar <- ancestors(y, observed.graph(G.xbar), topo)
  w <- setdiff(setdiff(v, x), an.xbar)
  w.len <- length(w)
  if (w.len != 0) {
    nxt <- soid(y, union(x, w) %ts% topo, P, G, G.obs, I, J, v, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 3
    tree$call$w <- w
    tree$call$an.xbar <- an.xbar
    return(list(P = nxt$P, tree = tree))
  }

  # line 4
  G.remove.x <- igraph::induced_subgraph(G, v[!(v %in% x)])
  s <- c_components(G.remove.x, topo)
  if (length(s) > 1) {
    tree$call$line <- 4
    nxt <- lapply(s, function(t) {
      return(soid(t, setdiff(v, t), P, G, G.obs, I, J, v, topo, list()))
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
    cc <- c_components(G, topo)
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
        cond.set <- v[0:(ind[i]-1)]
        if (P$product) {
          P.prod <- parse.joint(P, s[i], cond.set, v, topo)
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
        P.prod$sumset <- union(P.prod$sumset, setdiff(s, y)) %ts% topo
        # P.prod <- simplify.expression(P.prod, NULL)
      } else {
        P.prod$var <- setdiff(P.prod$var, union(P.prod$sumset, setdiff(s, y)))
      }
      tree$root <- P.prod
      return(list(P = P.prod, tree = tree))
    }

    # line 7
    # browser()
    tree$call$s <- s
    s <- Find(function(x) all(s %in% x), cc)
    tree$call$line <- 7
    tree$call$s.prime <- s
    s.len <- length(s)
    ind <- which(v %in% s)
    G.s <- igraph::induced_subgraph(G, s)
    G.s.obs <- observed.graph(G.s)
    product.list <- vector(mode = "list", length = s.len)
    P.prod <- probability()
    for (i in s.len:1) {
      # cond.set <- causal.parents(s[i], v[1:ind[i]], G, G.obs, topo)
      cond.set <- v[0:(ind[i]-1)]
      if (P$product) {
        P.prod <- parse.joint(P, s[i], cond.set, v, topo)
      } else {
        P.prod <- P
        P.prod$var <- s[i]
        P.prod$cond <- cond.set
      }
      product.list[[s.len - i + 1]] <- P.prod
    }
    x.new <- intersect(x, s)
    nxt <- NULL
    if (s.len > 1) nxt <- soid(y, x.new, probability(product = TRUE, children = product.list), G.s, G.s.obs, I, J, s, topo, list())
    else nxt <- soid(y, x.new, product.list[[1]], G.s, G.s.obs, I, J, s, topo, list())
    tree$branch[[1]] <- nxt$tree
    return(list(P = nxt$P, tree = tree))
  }

}
