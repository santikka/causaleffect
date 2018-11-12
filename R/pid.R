pid <- function(y, x, P, G, G.obs, v, topo, tree) {
  to <- NULL
  from <- NULL
  description <- NULL
  if (length(P$var) == 0 & !(P$product | P$fraction)) tree$call <- list(y = y, x = x, P = probability(var = v), G = G, line = "", v = v, id = FALSE)
  else tree$call <- list(y = y, x = x, P = P, G = G, line = "", v = v, id = FALSE)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, y), P$sumset) %ts% topo
    } else {
      P$var <- y
    }
    tree$call$line <- 1
    tree$call$id <- TRUE
    tree$root <- P
    return(list(P = P, tree = tree))
  }

  an <- ancestors(y, G.obs, topo)

  # line 2
  if (length(setdiff(v, an)) != 0) {
    G.an <- induced.subgraph(G, an)
    G.an.obs <- observed.graph(G.an)
    if (P$product | P$fraction) {
      P$sumset <- union(setdiff(v, an), P$sumset) %ts% topo
    } else {
      P$var <- an
    }
    nxt <- pid(y, intersect(x, an), P, G.an, G.an.obs, an, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$id <- nxt$tree$call$id
    tree$call$an <- an
    return(list(P = nxt$P, tree = tree))
  }

  # line 3
  G.xbar <- subgraph.edges(G, E(G)[!(to(x) | (from(x) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
  co <- connected(y, G.xbar, topo)
  z <- setdiff(an, co)
  if (length(z) > 0) {
    v.new <- setdiff(v, z) %ts% topo
    G.z <- induced.subgraph(G, v.new)
    G.z.obs <- observed.graph(G.z)
    G.z.l <- latent.projection(G, z)
    if (compare.graphs(G.z, G.z.l)) {
      if (P$product | P$fraction) {
        P$sumset <- union(z, P$sumset) %ts% topo
      } else {
        P$var <- v.new
      }
      nxt <- pid(y, setdiff(x, z), P, G.z, G.z.obs, v.new, topo, list())
      tree$branch[[1]] <- nxt$tree
      tree$call$line <- 3
      tree$call$id <- nxt$tree$call$id
      tree$call$z <- z
      return(list(P = nxt$P, tree = tree))
    }
  }

  # line 4
  v.x <- setdiff(v, x)
  G.xbar.obs <- observed.graph(G.xbar)
  de <- descendants(x, G.obs, topo)
  t <- c()
  for (i in 1:length(v.x)) {
    r <- setdiff(ancestors(v.x[i], G.xbar.obs, topo), de)
    G.vbar <- subgraph.edges(G, E(G)[!(to(v.x[i]) | (from(v.x[i]) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
    co.vi <-  connected(setdiff(v, r), G.vbar, topo)
    t <- union(t, setdiff(r, co.vi))
  }
  if (length(t) > 0) {
    v.new <- setdiff(v, t) %ts% topo
    G.t <- induced.subgraph(G, v.new)
    G.t.obs <- observed.graph(G.t)
    if (P$product | P$fraction) {
      P$sumset <- union(t, P$sumset) %ts% topo
    } else {
      P$var <- v.new
    }
    nxt <- pid(y, x, P, G.t, G.t.obs, v.new, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 4
    tree$call$id <- nxt$tree$call$id
    tree$call$t <- t
    return(list(P = nxt$P, tree = tree))
  }

  # line 5
  if (length(x) == 1) {
    cc <- c.components(G, topo)
    s <- Find(function(z) x %in% z, cc)
    G.s <- induced.subgraph(G, s)
    G.s.obs <- observed.graph(G.s)
    ch <- setdiff(children(x, G.s.obs, topo), x)
    if (length(ch) == 0) {
      v.xy <- setdiff(v, c(x,y)) %ts% topo
      v.xy.len <- length(v.xy)
      proj <- FALSE
      if (v.xy.len > 0 ){
        for (i in 1:v.xy.len) {
          G.prime <- latent.projection(G, v.xy[i])
          cc.prime <- c.components(G.prime, topo)
          s.prime <- Find(function(z) x %in% z, cc.prime)
          G.s.prime <- induced.subgraph(G.prime, s.prime)
          G.s.prime.obs <- observed.graph(G.s.prime)
          ch.prime <- setdiff(children(x, G.s.prime.obs, topo), x)
          if (length(ch.prime) == 0) {
            G.prime.obs <- observed.graph(G.prime)
            v.new <- setdiff(v, v.xy[i])
            if (P$product | P$fraction) {
              P$sumset <- union(v.xy[i], P$sumset) %ts% topo
            } else {
              P$var <- v.new
            }
            G <- G.prime
            G.obs <- G.prime.obs
            v <- v.new
            proj <- TRUE
          }
        }
        if (proj) {
          nxt <- pid(y, x, P, G, G.obs, v, topo, list())
          tree$branch[[1]] <- nxt$tree
          tree$call$line <- 5
          tree$call$id <- nxt$tree$call$id
          tree$call$vi <- v.xy[i]
          return(list(P = nxt$P, tree = tree))
        }
      }
    }
  }

  # line 6
  an.xbar <- ancestors(y, observed.graph(G.xbar), topo)
  w <- setdiff(setdiff(v, x), an.xbar)
  w.len <- length(w)
  if (w.len != 0) {
    nxt <- pid(y, union(x, w) %ts% topo, P, G, G.obs, v, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 6
    tree$call$id <- nxt$tree$call$id
    tree$call$w <- w
    tree$call$an.xbar <- an.xbar
    return(list(P = nxt$P, tree = tree))
  }

  # line 7
  G.remove.x <- induced.subgraph(G, v[!(v %in% x)])
  s <- c.components(G.remove.x, topo)
  if (length(s) > 1) {
    tree$call$line <- 7
    nxt <- lapply(s, function(t) {
      return(pid(t, setdiff(v, t), P, G, G.obs, v, topo, list()))
    })
    product.list <- lapply(nxt, "[[", "P")
    tree$branch <- lapply(nxt, "[[", "tree")
    tree$call$id <- all(sapply(nxt, function(x) x$tree$call$id))
    return(list(
      P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = product.list),
      tree = tree
    ))
  } else {
    s <- s[[1]]

    # line 8
    cc <- c.components(G, topo)
    if (identical(cc[[1]], v)) {
      tree$call$s <- cc[[1]]
      tree$call$line <- 8
      tree$call$id <- FALSE
      tree$root <- P
      return(list(P = P, tree = tree))
    }
   
    # line 9
    pos <- Position(function(x) identical(s, x), cc, nomatch = 0)
    if (pos > 0) {
      tree$call$line <- 9
      tree$call$s <- s
      ind <- which(v %in% s)
      s.len <- length(s)
      product.list <- vector(mode = "list", length = s.len)
      P.prod <- probability()
      for (i in s.len:1) {
        # cond.set <- causal.parents(s[i], v[1:ind[i]], G, G.obs, topo)
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
        tree$call$id <- TRUE
        return(list(P = P.new, tree = tree))
      } 
      if (P.prod$product | P.prod$fraction) {
        P.prod$sumset <- union(P.prod$sumset, setdiff(s, y)) %ts% topo
        # P.prod <- simplify.expression(P.prod, NULL)
      } else {
        P.prod$var <- setdiff(P.prod$var, union(P.prod$sumset, setdiff(s, y)))
      }
      tree$root <- P.prod
      tree$call$id <- TRUE
      return(list(P = P.prod, tree = tree))
    }

    # line 10
    tree$call$s <- s
    s <- Find(function(x) all(s %in% x), cc)
    tree$call$line <- 10
    tree$call$s.prime <- s
    s.len <- length(s)
    ind <- which(v %in% s)
    G.s <- induced.subgraph(G, s)
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
    if (s.len > 1) nxt <- pid(y, x.new, probability(product = TRUE, children = product.list), G.s, G.s.obs, s, topo, list())
    else nxt <- pid(y, x.new, product.list[[1]], G.s, G.s.obs, s, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$id <- nxt$tree$call$id
    return(list(P = nxt$P, tree = tree))
  }

}
