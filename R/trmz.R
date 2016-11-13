trmz <-
function(y, x, P, J, domain, w.index, D, Z, to, tree) {
  d <- length(D)
  v.s <- lapply(D, function(x) get.vertex.attribute(x, "name"))
  s <- lapply(1:d, function(x) v.s[[x]][which(vertex.attributes(D[[x]])$description == "S")])
  s <- lapply(1:d, function(x) to[[x]][which(to[[x]] %in% s[[x]])])
  D.causal <- D[[1]]
  v <- get.vertex.attribute(D.causal, "name")
  v <- to[[1]][which(to[[1]] %in% v)]
  D.obs <- observed.graph(D.causal)
  D.s.obs <- lapply(D, function(x) observed.graph(x))
  anc <- ancestors(y, D.obs, to[[1]])
  anc.s <- lapply(1:d, function(x) ancestors(y, D.s.obs[[x]], to[[x]]))
  G.export <- D[[domain]]
  if (length(P$var) == 0 & !P$product & !P$fraction) tree$call <- list(y = y, x = x, P = probability(var = v, domain = domain), 
    I = J, S = domain-1, G = G.export, line = "", v = v)
  else tree$call <- list(y = y, x = x, P = P, I = J, S = domain-1, G = G.export, line = "", v = v)

  # line 1
  if (length(x) == 0) {
    if (P$product | P$fraction | P$sum) {
      P$sumset <- union(setdiff(v, y), P$sumset)
      P <- simplify.expression(P, NULL)
    } else {
      P$var <- y
    }
    tree$call$line <- 1
    tree$root <- P
    return(list(P = P, W = w.index, tree = tree))
  }

  # line 2
  if (length(setdiff(v, anc)) != 0) {
    anc.graph <- lapply(1:d, function(x) induced.subgraph(D[[x]], anc.s[[x]]))
    if (P$product | P$fraction | P$sum) {
      P$sumset <- union(setdiff(v, anc), P$sumset)
      P <- simplify.expression(P, NULL)
    } else {
      P$var <- anc
    }
    nxt <- trmz(y, intersect(x, anc), P, J, domain, w.index, anc.graph, Z, to, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 2
    tree$call$anc <- anc
    return(list(P = nxt$P, W = nxt$W, tree = tree))
  }

  # line 3
  D.x.overbar <- subgraph.edges(D.causal, E(D.causal)[!to(x)], delete.vertices = FALSE)
  anc.xbar <- ancestors(y, observed.graph(D.x.overbar), to[[1]])
  w <- setdiff(setdiff(v, x), anc.xbar)
  if (length(w) != 0) {
    nxt <- trmz(y, union(x, w), P, J, domain, w.index, D, Z, to, list())
    tree$branch[[1]] <- nxt$tree
    tree$call$line <- 3
    tree$call$w <- w
    tree$call$anc.xbar <- anc.xbar
    return(list(P = nxt$P, W = nxt$W, tree = tree))
  } 

  # line 4
  D.remove.x <- induced.subgraph(D.causal, v[!(v %in% x)])
  cc <- c.components(D.remove.x, to[[1]])
  if (length(cc) > 1) {
    tree$call$line <- 4
    productlist <- list()
    nxt.list <- list()
    for (i in 1:length(cc)) {
      if (i == 1) nxt.list[[1]] <- trmz(cc[[1]], setdiff(v, cc[[1]]), P, J, domain, w.index, D, Z, to, list())
      else nxt.list[[i]] <- trmz(cc[[i]], setdiff(v, cc[[i]]), P, J, domain, nxt.list[[i-1]]$W, D, Z, to, list())
      productlist[[i]] <- nxt.list[[i]]$P
      tree$branch[[i]] <- nxt.list[[i]]$tree
    }
    return(list(
      P = probability(sumset = setdiff(v, union(y, x)), product = TRUE, children = productlist),
      W = nxt.list[[length(cc)]]$W,
      tree = tree
    ))
  }

  # line 5
  else {
    cc <- cc[[1]]
    cG.s <- lapply(1:d, function(x) sc.components(D[[x]], to[[x]]))
    cG <- cG.s[[1]]

    # line 6
    if (!identical(cG[[1]], v)) {
      is.element <- FALSE
      for (i in 1:length(cG)) {
        if (identical(cc, cG[[i]])) {
          is.element <- TRUE
          break
        }
      }

      # line 7
      if (is.element) {
        P.new <- probability()
        ind <- which(v %in% cc)
        productlist <- list()
        tree$call$line <- 7
        tree$call$c.zero <- cc
        for (i in 1:length(cc)) {
          P.prod <- probability()
          P.num <- P
          P.den <- P
          if (P$product) {
            P.num$sumset <- union(P.num$sumset, setdiff(v, v[0:ind[i]]))
            P.den$sumset <- union(P.den$sumset, setdiff(v, v[0:(ind[i]-1)]))
            P.prod <- simplify.expression(P.num, P.den)
          } else {
            P.prod <- P
            P.prod$var <- v[ind[i]]
            P.prod$cond <- v[0:(ind[i]-1)]
            #if (length(P.den$var) > 0) {
            #  P.num$fraction <- TRUE
            #  P.num$divisor <- P.den
            #}
          }
          productlist[[i]] <- P.prod
        }  
        if (length(productlist) > 1) P.new <- probability(sumset = setdiff(cc, y), product = TRUE, children = productlist)
        else {
          P.new <- productlist[[1]]
          P.new$sumset <- union(P.new$sumset, setdiff(cc, y))
        }
        tree$root <- P.new
        return(list(P = P.new, W = w.index, tree = tree))
      }

      # line 8
      tree$call$c.zero <- cc
      cc.s <- lapply(cG.s, function(x) {
        x[[which(unlist(lapply(x, function(y) all(cc %in% y))))]]
      })
      cc <- cc.s[[1]]
      tree$call$line <- 8
      tree$call$c.prime <- cc
      productlist <- list()
      ind <- which(v %in% cc)
      cc.graph <- lapply(1:d, function(x) induced.subgraph(D[[x]], cc.s[[x]]))
      kappa <- c()
      if (length(cc) > 1) {
        for (i in 1:length(cc)) { 
          kappa <- union(kappa, setdiff(v[0:(ind[i]-1)], cc))
          if (P$product) {
            P.prod <- parse.joint(P, cc[i], union(intersect(v[0:(ind[i]-1)], cc), kappa), v)
            P.prod <- simplify.expression(P.prod, NULL)
            productlist[[i]] <- P.prod
          } else {
            P.prod <- P
            P.prod$var <- cc[i]
            P.prod$cond <- union(intersect(v[0:(ind[i]-1)], cc), kappa) 
            productlist[[i]] <- P.prod
          }
        }
        nxt <- trmz(y, intersect(x, cc), probability(product = TRUE, children = productlist), J, domain, w.index, cc.graph, Z, to, list())
        tree$branch[[1]] <- nxt$tree
        return(list(P = nxt$P, W = nxt$W, tree = tree))
      } else {
        kappa <- setdiff(v[0:(ind[1]-1)], cc)
        if (P$product) {
          P.prod <- parse.joint(P, cc[i], union(intersect(v[0:(ind[i]-1)], cc), kappa) , v)
          P.prod <- simplify.expression(P.prod, NULL)
          nxt <- trmz(y, intersect(x, cc), P.prod, J, domain, w.index, cc.graph, Z, to, list())
          tree$branch[[1]] <- nxt$tree
          return(list(P = nxt$P, W = nxt$W, tree = tree))
        } else {
          P.prod <- P
          P.prod$var <- cc[1]
          P.prod$cond <- union(intersect(v[0:(ind[1]-1)], cc), kappa) 
          nxt <- trmz(y, intersect(x, cc), P.prod, J, domain, w.index, cc.graph, Z, to, list())
          tree$branch[[1]] <- nxt$tree
          return(list(P = nxt$P, W = nxt$W, tree = tree))
        }
      }

    # line 9
    } else {

      # line 10
      E.tr <- list()
      if (length(J) == 0) {
        tree$call$line <- 10
        ind <- 0
        W.new <- w.index + 1
        for (i in 1:length(D)) {
          A <- unobserved.graph(D[[i]])
          A <- subgraph.edges(A, E(A)[!to(x)], delete.vertices = FALSE)
          A <- as.matrix(get.adjacency(A))
          if (wrap.dSep(A, s[[i]], y, x) & (length(intersect(Z[[i]], x)) != 0)) {
            P$domain <- i
            ind <- ind + 1
            xcapz <- intersect(Z[[i]], x)
            D.remove.xcapz <- lapply(1:d, function(x) induced.subgraph(D[[x]], v[!(v %in% xcapz)]))
            nxt <- trmz(y, setdiff(x, Z[[i]]), P, xcapz, i, W.new, D.remove.xcapz, Z, to, list())
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
        P.new = probability(sum = TRUE, children = E.tr, weight = w.index)
        tree$root <- P.new
        return(list(P = P.new, W = W.new, tree = tree))
      } 
      if (length(E.tr) == 1) {
        tree$root <- E.tr[[1]]
        return(list(P = E.tr[[1]], tree = tree))
      }
      stop("Not transportable.", call. = FALSE)
    }
  }  
}
