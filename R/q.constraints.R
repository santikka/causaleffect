q.constraints <- function(s, node, G, G.obs, to, constraints) {
  G.s <- induced.subgraph(G, s)
  G.s.obs <- observed.graph(G.s)
  desc.sets <- descendant.sets(node, s, G.s.obs, to)
  if (length(desc.sets) > 0) {
    for (d in desc.sets) {
      s_d <- setdiff(s, d)
      eff.d <- setdiff(parents(s, G.obs, to), d)
      eff.s_d <- parents(s_d, G.obs, to)
      ind <- which(to %in% s)
      prod <- probability()
      s.len <- length(s)
      product.list <- vector(mode = "list", length = s.len)
      for (i in 1:s.len) {
        prod$var <- s[i]
        prod$cond <- to[0:(ind[i]-1)]
        product.list[[i]] <- prod
      }
      q.factor <- probability(sumset = d, product = TRUE, children = product.list)
      eff.diff <- setdiff(eff.d, eff.s_d)
      if (length(eff.diff) > 0) {
        constraints <- c(constraints, list(c(
          "X" = get.expression(q.factor), 
          "Y" = paste0(eff.diff, collapse = ",")
        )))
      }
      d.prime <- s_d
      G.d <- induced.subgraph(G.s, d.prime)
      cc.d <- c.components(G.d, to)
      if (length(cc.d) == 1) return (constraints)
      e <- Find(function(x) node %in% x, cc.d)
      q.d.factor <- probability(fraction = TRUE)
      q.d.factor$num <- q.factor
      q.factor$sumset <- union(q.factor$sumset, node)
      q.d.factor$den <- q.factor
      eff.e <- parents(e, G.obs, to)
      eff.diff <- setdiff(eff.d, eff.e)
      print(constraints)
      if (length(eff.diff) > 0) {
        constraints <- c(constraints, list(c(
          "X" = get.expression(q.d.factor), 
          "Y" = paste0(eff.diff, collapse = ",")
        )))
      }
      return (q.constraints(e, node, G, G.obs, to, constraints))
    }
  } 
  return (constraints)
}