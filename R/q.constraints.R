q.constraints <- function(s, node, G, G.obs, G.unobs, to, to.u, constraints) {
  G.s <- induced.subgraph(G, s)
  v <- get.vertex.attribute(G, "name")
  v <- v %ts% to
  G.s.obs <- observed.graph(G.s)
  desc.sets <- descendent.sets(node, s, G.s.obs, to)
  e <- NULL
  if (length(desc.sets) > 0) {
    for (d in desc.sets) {
      s_d <- setdiff(s, d)
      s.pa <- parents(s, G.obs, to)
      eff.d <- setdiff(s.pa, d)
      eff.s_d <- parents(s_d, G.obs, to)
      prod <- probability()

      # C factor of the left hand side
      ind <- which(to %in% s)
      s.len <- length(s)
      product.list <- vector(mode = "list", length = s.len)
      for (i in s.len:1) {
        prod$var <- s[i]
        prod$cond <- to[0:(ind[i]-1)]
        product.list[[s.len - i + 1]] <- prod
      }
      q.factor <- probability(sumset = d, product = TRUE, children = product.list)
      q.factor.rhs <- NULL

      # C factor of the right hand side
      u <- V(G.unobs)[which(vertex.attributes(G.unobs)$description == "U")]$name
      u.pa <- NULL
      s_d.len <- length(s_d)
      product.list.rhs <- vector(mode = "list", length = s_d.len)
      for (i in s_d.len:1) {
        prod$var <- s_d[i]
        pa <- setdiff(parents(s_d[i], G.unobs, to.u), s_d[i])
        cond.unobs <- pa %ts% u
        cond.obs <- setdiff(pa, cond.unobs)
        prod$cond <- c(cond.obs, cond.unobs)
        u.pa <- c(u.pa, cond.unobs)
        product.list.rhs[[s_d.len - i + 1]] <- prod
      }
      u.pa <- unique(u.pa)
      u.pa.len <- length(u.pa)
      prod <- probability()
      if (u.pa.len > 0) {
        for (i in u.pa.len:1) {
          prod$var <- u.pa[i]
          product.list.rhs[[s_d.len + u.pa.len - i + 1]] <- prod
        }
        q.factor.rhs <- probability(product = TRUE, sumset = u.pa, children = product.list.rhs)
      } else {
        q.factor.rhs <- probability(product = TRUE, children = product.list.rhs)
      }
      rhs.text <- paste0("Q[\\{", paste0(s_d, collapse = ","), "\\}](", paste0(eff.s_d, collapse = ","), ")", collapse = "")

      eff.diff <- setdiff(eff.d, eff.s_d)
      if (length(eff.diff) > 0) {
        constraints <- c(constraints, list(list(
          "rhs.cfactor" = rhs.text,
          "rhs.expr" = get.expression(q.factor.rhs),
          "lhs.cfactor" = paste0("\\sum_{", paste0(d, collapse = ","), "}Q[\\{", paste0(s, collapse = ","), "\\}](", 
            paste0(s.pa, collapse = ","), ")", collapse = ""),
          "lhs.expr" = get.expression(q.factor),
          "vars" = eff.diff
        )))
      }
      d.prime <- s_d
      G.d <- induced.subgraph(G.s, d.prime)
      v <- get.vertex.attribute(G.d, "name")
      v <- v %ts% to
      cc.d <- c.components(G.d, to)
      if (length(cc.d) > 1) e <- Find(function(x) node %in% x, cc.d)
      else e <- cc.d[[1]]

      # C factor of the left hand side
      
      q.d.factor <- probability(fraction = TRUE)
      q.d.factor$num <- q.factor
      q.factor$sumset <- union(q.factor$sumset, node) %ts% to
      q.d.factor$den <- q.factor
      q.factor.lhs <- NULL

      # C factor of the right hand side
      u.pa <- NULL
      e.len <- length(e)
      product.list.rhs <- vector(mode = "list", length = e.len)
      for (i in e.len:1) {
        prod$var <- e[i]
        pa <- setdiff(parents(e[i], G.unobs, to.u), e[i])
        cond.unobs <- pa %ts% u
        cond.obs <- setdiff(pa, cond.unobs)
        prod$cond <- c(cond.obs, cond.unobs)
        u.pa <- c(u.pa, cond.unobs)
        product.list.rhs[[e.len - i + 1]] <- prod
      }
      u.pa <- unique(u.pa)
      u.pa.len <- length(u.pa)
      prod <- probability()
      if (u.pa.len > 0) {
        for (i in u.pa.len:1) {
          prod$var <- u.pa[i]
          product.list.rhs[[e.len + u.pa.len - i + 1]] <- prod
        }
        q.factor.rhs <- probability(product = TRUE, sumset = u.pa, children = product.list.rhs)
      } else {
        q.factor.rhs <- probability(product = TRUE, children = product.list.rhs)
      }

      eff.e <- parents(e, G.obs, to)
      eff.diff <- setdiff(eff.s_d, eff.e)
      if (length(eff.diff) > 0) {
        constraints <- c(constraints, list(list(
          "rhs.cfactor" = paste0("Q[\\{", paste0(e, collapse = ","), "\\}](", paste0(eff.e, collapse = ","), ")", collapse = ""),
          "rhs.expr" = get.expression(q.factor.rhs),
          "lhs.cfactor" = paste0("\\frac{", rhs.text, "}{\\sum_{", node, "}", rhs.text, "}", collapse = ""),
          "lhs.expr" = get.expression(q.d.factor),
          "vars" = eff.diff
        )))
      }
      constraints <- c(constraints, q.constraints(e, node, G, G.obs, G.unobs, to, to.u, list()))
    }
  }
  return(constraints)
}