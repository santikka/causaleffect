trso <- function(y, x, P, J, domain, D, Z, topo, tree) {
  to <- NULL
  from <- NULL
  description <- NULL
  d <- length(D)
  v.s <- lapply(D, function(x) igraph::get.vertex.attribute(x, "name"))
  s <- lapply(1:d, function(x) v.s[[x]][which(igraph::vertex.attributes(D[[x]])$description == "S")])
  s <- lapply(1:d, function(x) topo[[x]][which(topo[[x]] %in% s[[x]])])
  D.causal <- D[[1]]
  v <- igraph::get.vertex.attribute(D.causal, "name")
  v <- topo[[1]][which(topo[[1]] %in% v)]
  D.obs <- observed.graph(D.causal)
  D.s.obs <- lapply(D, function(x) observed.graph(x))
  anc <- ancestors(y, D.obs, topo[[1]])
  anc.s <- lapply(1:d, function(x) ancestors(y, D.s.obs[[x]], topo[[x]]))
  G.export <- D[[domain]]
  if (length(P$var) == 0 & !P$product & !P$fraction) tree$call <- list(y = y, x = x, P = probability(var = v, domain = domain),
    I = J, S = domain-1, G = G.export, line = "", v = v, Z = Z, id = FALSE)
  else tree$call <- list(y = y, x = x, P = P, I = J, S = domain-1, G = G.export, line = "", v = v, Z = Z, id = FALSE)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction | P$sum) {
      P$sumset <- union(setdiff(v, y), P$sumset)
      P <- simplify.expression(P, NULL)
    } else {
      P$var <- y
    }
    tree$call$line <- 1
    tree$call$id <- TRUE
    tree$root <- P
    return(list(P = P, tree = tree))
  }

  # line 2
  if (length(setdiff(v, anc)) != 0) {
    anc.graph <- lapply(1:d, function(x) igraph::induced.subgraph(D[[x]], anc.s[[x]]))
    if (P$product | P$fraction | P$sum) {
      P$sumset <- union(setdiff(v, anc), P$sumset)
      P <- simplify.expression(P, NULL)
    } else {
      P$var <- anc
    }
    nxt <- trso(y, intersect(x, anc), P, J, domain, anc.graph, Z, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$id <- nxt$tree$call$id
    tree$call$anc <- anc
    return(list(P = nxt$P, tree = tree))
  }

  # line 3
  D.x.overbar <- igraph::subgraph.edges(D.causal, igraph::E(D.causal)[!(to(x) | (from(x) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
  anc.xbar <- ancestors(y, observed.graph(D.x.overbar), topo[[1]])
  w <- setdiff(setdiff(v, x), anc.xbar)
  if (length(w) != 0) {
    nxt <- trso(y, union(x, w), P, J, domain, D, Z, topo, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 3
    tree$call$id <- nxt$tree$call$id
    tree$call$w <- w
    tree$call$anc.xbar <- anc.xbar
    return(list(P = nxt$P, tree = tree))
  }

  # line 4
  D.remove.x <- igraph::induced.subgraph(D.causal, v[!(v %in% x)])
  cc <- c.components(D.remove.x, topo[[1]])
  cc.len <- length(cc)
  if (cc.len > 1) {
    tree$call$line <- 4
    product.list <- vector(mode = "list", length = cc.len)
    nxt.list <- vector(mode = "list", length = cc.len)
    id.list <- logical(length = cc.len)
    for (i in 1:cc.len) {
      if (i == 1) nxt.list[[1]] <- trso(cc[[1]], setdiff(v, cc[[1]]), P, J, domain, D, Z, topo, list())
      else nxt.list[[i]] <- trso(cc[[i]], setdiff(v, cc[[i]]), P, J, domain, D, Z, topo, list())
      product.list[[i]] <- nxt.list[[i]]$P
      tree$branch[[i]] <- nxt.list[[i]]$tree
      id.list[i] <- nxt.list[[i]]$tree$call$id
    }
    tree$call$id <- all(id.list)
    return(list(P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = product.list), tree = tree))
  }

  # line 5
  else {

    # line 6
    E.tr <- list()
    if (length(J) == 0) {
      tree$call$line <- 6
      ind <- 0
      for (i in 2:length(D)) {
        D.unobs <- unobserved.graph(D[[i]])
        D.unobs <- igraph::subgraph.edges(D.unobs, igraph::E(D.unobs)[!to(x)], delete.vertices = FALSE)
        if (wrap.dSep(D.unobs, s[[i]], y, x) & (length(intersect(Z[[i]], x)) > 0)) {
          P.new <- P
          P.new$domain <- i
          xcapz <- intersect(Z[[i]], x)
          D.remove.xcapz <- lapply(1:d, function(x) induced.subgraph(D[[x]], v.s[[x]][!(v.s[[x]] %in% xcapz)]))
          nxt <- trso(y, setdiff(x, Z[[i]]), P.new, xcapz, i, D.remove.xcapz, Z, topo, list())
          if (nxt$tree$call$id) {
            ind <- ind + 1
            tree$branch[[ind]] <- nxt$tree
            E.new <- nxt$P
            E.new <- activate.interventions(E.new, i, xcapz)
            E.tr[[ind]] <- E.new
          }
        }
      }

      # line 7
      if (length(E.tr) >= 1) {
        tree$root <- E.tr[[1]]
        tree$call$id <- TRUE
        return(list(P = E.tr[[1]], tree = tree))
      }
    }

    cc <- cc[[1]]
    cG.s <- lapply(1:d, function(x) sc.components(D[[x]], topo[[x]]))
    cG <- cG.s[[1]]

    # line 8
    if (!identical(cG[[1]], v)) {
      pos <- Position(function(x) identical(cc, x), cG, nomatch = 0)

      # line 9
      if (pos > 0) {
        P.new <- probability()
        ind <- which(v %in% cc)
        cc.len <- length(cc)
        product.list <- vector(mode = "list", length = cc.len)
        tree$call$line <- 9
        tree$call$czero <- cc
        for (i in 1:cc.len) {
          P.prod <- probability()
          P.num <- P
          P.den <- P
          if (P$product) {
            P.num$sumset <- union(P.num$sumset, setdiff(v, v[0:ind[i]]))
            P.den$sumset <- union(P.den$sumset, setdiff(v, v[0:(ind[i]-1)]))
            P.prod <- probability(fraction = TRUE, num = P.num, den = P.den)
            P.prod <- simplify.expression(P.num, P.den)
          } else {
            P.prod <- P
            P.prod$var <- v[ind[i]]
            P.prod$cond <- v[0:(ind[i]-1)]
          }
          product.list[[cc.len - i + 1]] <- P.prod
        }
        if (cc.len > 1) P.new <- probability(sumset = setdiff(cc, y), product = TRUE, children = product.list)
        else {
          P.new <- product.list[[1]]
          P.new$sumset <- union(P.new$sumset, setdiff(cc, y))
        }
        tree$root <- P.new
        tree$call$id <- TRUE
        return(list(P = P.new, tree = tree))
      }

      # line 10
      tree$call$czero <- cc
      cc.s <- lapply(cG.s, function(x) {
        x[[which(unlist(lapply(x, function(y) all(cc %in% y))))]]
      })
      cc <- cc.s[[1]]
      cc.len <- length(cc)
      product.list <- vector(mode = "list", length = cc.len)
      tree$call$line <- 10
      tree$call$c.prime <- cc
      ind <- which(v %in% cc)
      cc.graph <- lapply(1:d, function(x) induced.subgraph(D[[x]], cc.s[[x]]))
      Z.prime <- NULL
      s.cc <- lapply(1:d, function(x) intersect(s[[x]], cc.s[[x]]))
      if (length(J) != 0) {
        if (length(s.cc[[domain]]) == 0) Z.prime <- Z
        else {
          tree$call$line <- 10
          tree$call$czero <- cc
          tree$call$id <- FALSE
          tree$root <- P
          return(list(P = P, tree = tree))
        }
      } else {
        Z.prime <- vector(mode = "list", length = d)
      }
      kappa <- c()
      if (cc.len > 1) {
        for (i in 1:cc.len) {
          kappa <- union(kappa, setdiff(v[0:(ind[i]-1)], cc))
          if (P$product) {
            P.prod <- parse.joint(P, cc[i], union(intersect(v[0:(ind[i]-1)], cc), kappa), v, topo)
            product.list[[cc.len - i + 1]] <- P.prod
          } else {
            P.prod <- P
            P.prod$var <- cc[i]
            P.prod$cond <- union(intersect(v[0:(ind[i]-1)], cc), kappa) 
            product.list[[cc.len - i + 1]] <- P.prod
          }
        }
        nxt <- trso(y, intersect(x, cc), probability(product = TRUE, children = product.list), J, domain, cc.graph, Z.prime, topo, list())
        tree$branch[[1]] <- nxt$tree
        tree$call$id <- nxt$tree$call$id
        return(list(P = nxt$P, tree = tree))
      } else {
        kappa <- setdiff(v[0:(ind[1]-1)], cc)
        if (P$product) {
          P.prod <- parse.joint(P, cc, union(intersect(v[0:(ind[i]-1)], cc), kappa) , v, topo)
          nxt <- trso(y, intersect(x, cc), P.prod, J, domain, cc.graph, Z.prime, topo, list())
          tree$branch[[1]] <- nxt$tree
          tree$call$id <- nxt$tree$call$id
          return(list(P = nxt$P, tree = tree))
        } else {
          P.prod <- P
          P.prod$var <- cc
          P.prod$cond <- union(intersect(v[0:(ind[1]-1)], cc), kappa)
          nxt <- trso(y, intersect(x, cc), P.prod, J, domain, cc.graph, Z.prime, topo, list())
          tree$branch[[1]] <- nxt$tree
          tree$call$id <- nxt$tree$call$id
          return(list(P = nxt$P, tree = tree))
        }
      }
    # line 11
    } else {
      tree$call$line <- 11
      tree$call$czero <- cc
      tree$call$id <- FALSE
      tree$root <- P
      return(list(P = P, tree = tree))
    }
  }
}
