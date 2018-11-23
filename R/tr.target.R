tr.target <- function(Z, W, G.obs, G, topo.obs) {
    # return(setdiff(descendants(Z, G.obs, topo.obs), W))
    target <- setdiff(descendants(Z, G.obs, topo.obs), W)
    #G.xbar <- subgraph.edges(G, E(G)[!(to(Z) | (from(Z) & (description == "U" & !is.na(description))))], delete.vertices = FALSE) # remove id nonid
    #nontarget <- setdiff(ancestors(W, G.xbar, topo.obs), target) # remove if nonid
    cc <- c.components(G, topo.obs)
    for (Wi in W) {
      target <- union(target, Find(function(x) Wi %in% x, cc))
    }
    #target <- setdiff(target, nontarget) # remove if nonid
    return(target %ts% topo.obs)
}