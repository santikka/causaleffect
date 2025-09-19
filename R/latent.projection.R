latent.projection <- function(G, l) {
  .to <- NULL
  .from <- NULL
  description <- NULL
  for (i in 1:length(l)) {
    e <- igraph::E(G)
    v <- igraph::vertex_attr(G, "name")
    inc.edges <- e[.to(l[i]) & (is.na(description) | description != "U")]
    out.edges <- e[.from(l[i]) & (is.na(description) | description != "U")]
    unobs.edges <- e[.to(l[i]) & description == "U" & !is.na(description)]
    inc.ind <- igraph::get.edges(G, inc.edges)[ ,1]
    out.ind <- igraph::get.edges(G, out.edges)[ ,2]
    unobs.ind <- setdiff(igraph::get.edges(G, unobs.edges)[ ,1], out.ind)
    inc.len <- length(inc.ind)
    out.len <- length(out.ind)
    unobs.len <- length(unobs.ind)
    if (inc.len > 0 & out.len > 0) {
      obs.new <- t(as.matrix(expand.grid(inc.ind, out.ind)))
      G <- G + igraph::edges(v[c(obs.new)], description = rep(NA, ncol(obs.new))) # replace path v_1 -> L -> v_2 with v_1 -> v_2
    }
    if (out.len > 1) {
      unobs.new <- combn(out.ind, 2)
      G <- G + igraph::edges(v[c(unobs.new, unobs.new[2:1, ])], description = rep("U", 2 * ncol(unobs.new))) # replace path v_1 <- L -> v_2 with v_1 <-> v_2
    }
    if (unobs.len > 0 & out.len > 0) {
      unobs.old <- t(as.matrix(expand.grid(unobs.ind, out.ind)))
      G <- G + igraph::edges(v[c(unobs.old, unobs.old[2:1, ])], description = rep("U", 2 * ncol(unobs.old))) # replace path v_1 <-> L -> v_2 with v_1 <-> v_2
    }
    G <- igraph::induced_subgraph(G, setdiff(v, l[i]))
    e.dat <- as.data.frame(igraph::get.edges(G, E(G)))
    e.dat[ ,3] <- igraph::edge_attr(G)
    G <- igraph::subgraph_from_edges(G, which(!duplicated(e.dat)), delete.vertices = FALSE)
  }
  return(G)
}
