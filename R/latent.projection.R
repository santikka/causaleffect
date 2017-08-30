latent.projection <- function(G, l) {
  to <- NULL
  for (i in 1:length(l)) {
    e <- E(G)
    v <- get.vertex.attribute(G, "name")
    inc.edges <- e[to(l[i]) & (is.na(description) | description != "U")]
    out.edges <- e[from(l[i]) & (is.na(description) | description != "U")]
    unobs.edges <- e[to(l[i]) & description == "U" & !is.na(description)]
    inc.ind <- get.edges(G, inc.edges)[ ,1]
    out.ind <- get.edges(G, out.edges)[ ,2]
    unobs.ind <- setdiff(get.edges(G, unobs.edges)[ ,1], out.ind)
    inc.len <- length(inc.ind)
    out.len <- length(out.ind)
    unobs.len <- length(unobs.ind)
    if (inc.len > 0 & out.len > 0) {
      obs.new <- t(as.matrix(expand.grid(inc.ind, out.ind)))
      G <- G + edges(v[c(obs.new)]) # replace path v_1 -> L -> v_2 with v_1 -> v_2
    }
    if (out.len > 1) {
      unobs.new <- combn(out.ind, 2)
      G <- G + edges(v[c(unobs.new, unobs.new[2:1, ])], description = rep("U", 2 * ncol(unobs.new))) # replace path v_1 <- L -> v_2 with v_1 <-> v_2
    }
    if (unobs.len > 0 & out.len > 0) {
      unobs.old <- t(as.matrix(expand.grid(unobs.ind, out.ind)))
      G <- G + edges(v[c(unobs.old, unobs.old[2:1, ])], description = rep("U", 2 * ncol(unobs.old))) # replace path v_1 <-> L -> v_2 with v_1 <-> v_2
    }
    G <- induced.subgraph(G, setdiff(v, l[i]))
    e.dat <- as.data.frame(get.edges(G, E(G)))
    e.dat[ ,3] <- edge.attributes(G)
    G <- subgraph.edges(G, which(!duplicated(e.dat)))
  }
  return(G)
}