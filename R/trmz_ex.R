trmz_ex <- function(y, x, P, J, domain, w.index, D, Z, topo, tree, prioritize) {
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
    return(list(P = P, W = w.index, tree = tree))
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
    nxt <- trmz_ex(y, intersect(x, anc), P, J, domain, w.index, anc.graph, Z, topo, list(), prioritize)
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$id <- nxt$tree$call$id
    tree$call$anc <- anc
    return(list(P = nxt$P, W = nxt$W, tree = tree))
  }

  # line 3
  D.x.overbar <- igraph::subgraph.edges(D.causal, igraph::E(D.causal)[!(to(x) | (from(x) & (description == "U" & !is.na(description))))], delete.vertices = FALSE)
  anc.xbar <- ancestors(y, observed.graph(D.x.overbar), topo[[1]])
  w <- setdiff(setdiff(v, x), anc.xbar)
  if (length(w) != 0) {
    nxt <- trmz_ex(y, union(x, w), P, J, domain, w.index, D, Z, topo, list(), prioritize)
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 3
    tree$call$id <- nxt$tree$call$id
    tree$call$w <- w
    tree$call$anc.xbar <- anc.xbar
    return(list(P = nxt$P, W = nxt$W, tree = tree))
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
      if (i == 1) nxt.list[[1]] <- trmz_ex(cc[[1]], setdiff(v, cc[[1]]), P, J, domain, w.index, D, Z, topo, list(), prioritize)
      else nxt.list[[i]] <- trmz_ex(cc[[i]], setdiff(v, cc[[i]]), P, J, domain, nxt.list[[i-1]]$W, D, Z, topo, list(), prioritize)
      product.list[[i]] <- nxt.list[[i]]$P
      tree$branch[[i]] <- nxt.list[[i]]$tree
      id.list[i] <- nxt.list[[i]]$tree$call$id
    }
    tree$call$id <- all(id.list)
    return(list(
      P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = product.list),
      W = nxt.list[[length(cc)]]$W,
      tree = tree
    ))
  }

  # line 5
  else {

    if (prioritize) {

      # line 10
      E.tr <- list()
      if (length(J) == 0) {
        tree$call$line <- 10
        ind <- 0
        W.new <- w.index + 1
        for (i in 1:length(D)) {
          D.unobs <- unobserved.graph(D[[i]])
          D.unobs <- igraph::subgraph.edges(D.unobs, igraph::E(D.unobs)[!to(x)], delete.vertices = FALSE)
          if (wrap.dSep(D.unobs, s[[i]], y, x) & (length(intersect(Z[[i]], x)) != 0)) {
            P.new <- P
            P.new$domain <- i
            xcapz <- intersect(Z[[i]], x)
            D.remove.xcapz <- lapply(1:d, function(x) igraph::induced.subgraph(D[[x]], v.s[[x]][!(v.s[[x]] %in% xcapz)]))
            nxt <- trmz_ex(y, setdiff(x, Z[[i]]), P, xcapz, i, W.new, D.remove.xcapz, Z, topo, list(), prioritize)
            if (nxt$tree$call$id) {
              ind <- ind + 1
              tree$branch[[ind]] <- nxt$tree
              E.new <- nxt$P
              W.new <- nxt$W
              E.new <- activate.interventions(E.new, i, xcapz)
              E.tr[[ind]] <- E.new
            }
          }
        }

        # line 11
        if (length(E.tr) > 1) {
          P.new <- probability(sum = TRUE, children = E.tr, weight = w.index)
          tree$root <- P.new
          tree$call$id <- TRUE
          return(list(P = P.new, W = W.new, tree = tree))
        }
        if (length(E.tr) == 1) {
          tree$root <- E.tr[[1]]
          tree$call$id <- TRUE
          return(list(P = E.tr[[1]], W = W.new, tree = tree))
        }
      }
    }

    cc <- cc[[1]]
    cG.s <- lapply(1:d, function(x) sc.components(D[[x]], topo[[x]]))
    cG <- cG.s[[1]]

    # line 6
    if (!identical(cG[[1]], v)) {
      pos <- Position(function(x) identical(cc, x), cG, nomatch = 0)

      # line 7
      if (pos > 0) {
        P.new <- probability()
        ind <- which(v %in% cc)
        cc.len <- length(cc)
        product.list <- vector(mode = "list", length = cc.len)
        tree$call$line <- 7
        tree$call$c.zero <- cc
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
        return(list(P = P.new, W = w.index, tree = tree))
      }

      # line 8

      tree$call$c.zero <- cc
      cc.s <- lapply(cG.s, function(x) {
        x[[which(unlist(lapply(x, function(y) all(cc %in% y))))]]
      })
      cc <- cc.s[[1]]
      cc.len <- length(cc)
      product.list <- vector(mode = "list", length = cc.len)
      tree$call$line <- 8
      tree$call$cprime <- cc
      ind <- which(v %in% cc)
      cc.graph <- lapply(1:d, function(x) igraph::induced.subgraph(D[[x]], cc.s[[x]]))
      kappa <- c()
      if (cc.len > 1) {
        for (i in 1:cc.len) { 
          kappa <- union(kappa, setdiff(v[0:(ind[i]-1)], cc))
          if (P$product) {
            P.prod <- parse.joint(P, cc[i], union(intersect(v[0:(ind[i]-1)], cc), kappa), v, topo)
            P.prod <- simplify.expression(P.prod, NULL)
            product.list[[cc.len - i + 1]] <- P.prod
          } else {
            P.prod <- P
            P.prod$var <- cc[i]
            P.prod$cond <- union(intersect(v[0:(ind[i]-1)], cc), kappa) 
            product.list[[cc.len - i + 1]] <- P.prod
          }
        }
        nxt <- trmz_ex(y, intersect(x, cc), probability(product = TRUE, children = product.list), J, domain, w.index, cc.graph, Z, topo, list(), prioritize)
        tree$branch[[1]] <- nxt$tree
        tree$call$id <- nxt$tree$call$id
        return(list(P = nxt$P, W = nxt$W, tree = tree))
      } else {
        kappa <- setdiff(v[0:(ind[1]-1)], cc)
        if (P$product) {
          P.prod <- parse.joint(P, cc, union(intersect(v[0:(ind[i]-1)], cc), kappa) , v, topo)
          P.prod <- simplify.expression(P.prod, NULL)
          nxt <- trmz_ex(y, intersect(x, cc), P.prod, J, domain, w.index, cc.graph, Z, topo, list(), prioritize)
          tree$branch[[1]] <- nxt$tree
          tree$call$id <- nxt$tree$call$id
          return(list(P = nxt$P, W = nxt$W, tree = tree))
        } else {
          P.prod <- P
          P.prod$var <- cc
          P.prod$cond <- union(intersect(v[0:(ind[1]-1)], cc), kappa)
          nxt <- trmz_ex(y, intersect(x, cc), P.prod, J, domain, w.index, cc.graph, Z, topo, list(), prioritize)
          tree$branch[[1]] <- nxt$tree
          tree$call$id <- nxt$tree$call$id
          return(list(P = nxt$P, W = nxt$W, tree = tree))
        }
      }

    # line 9
    } else {

      if (prioritize) {
        tree$call$czero <- cc
        tree$call$id <- FALSE
        tree$root <- P
        return(list(P = P, W = w.index, tree = tree))
      } else {

        # line 10
        E.tr <- list()
        if (length(J) == 0) {
          tree$call$line <- 10
          ind <- 0
          W.new <- w.index + 1
          for (i in 1:length(D)) {
            D.unobs <- unobserved.graph(D[[i]])
            D.unobs <- igraph::subgraph.edges(D.unobs, igraph::E(D.unobs)[!to(x)], delete.vertices = FALSE)
            if (wrap.dSep(D.unobs, s[[i]], y, x) & (length(intersect(Z[[i]], x)) != 0)) {
              P.new <- P
              P.new$domain <- i
              xcapz <- intersect(Z[[i]], x)
              D.remove.xcapz <- lapply(1:d, function(x) igraph::induced.subgraph(D[[x]], v.s[[x]][!(v.s[[x]] %in% xcapz)]))
              nxt <- trmz_ex(y, setdiff(x, Z[[i]]), P, xcapz, i, W.new, D.remove.xcapz, Z, topo, list(), prioritize)
              if (nxt$tree$call$id) {
                ind <- ind + 1
                tree$branch[[ind]] <- nxt$tree
                E.new <- nxt$P
                W.new <- nxt$W
                E.new <- activate.interventions(E.new, i, xcapz)
                E.tr[[ind]] <- E.new
              }
            }
          }

          # line 11
          if (length(E.tr) > 1) {
            P.new <- probability(sum = TRUE, children = E.tr, weight = w.index)
            tree$root <- P.new
            tree$call$id <- TRUE
            return(list(P = P.new, W = W.new, tree = tree))
          }
          if (length(E.tr) == 1) {
            tree$root <- E.tr[[1]]
            tree$call$id <- TRUE
            return(list(P = E.tr[[1]], W = W.new, tree = tree))
          }
        }

        tree$call$czero <- cc
        tree$call$id <- FALSE
        tree$root <- P
        return(list(P = P, W = w.index, tree = tree))
      }
    }
  }
}
