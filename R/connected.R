connected <- function(y, G, topo) {
  if (caugi::is_caugi(G)) {
    if (length(y) == 0L) {
      return(character(0))
    }
    e <- as.data.frame(caugi::edges(G))
    e <- unique(data.frame(from = pmin(e$from, e$to), to = pmax(e$from, e$to)))
    skeleton <- caugi::caugi(
      from = e$from,
      edge = rep("---", nrow(e)),
      to = e$to,
      nodes = caugi::nodes(G)$name,
      class = "PDAG"
    )
    co <- caugi::anteriors(skeleton, y, open = FALSE)
    return(unique(as.character(unlist(co, use.names = FALSE))) %ts% topo)
  }
  connected <- unique(unlist(igraph::neighborhood(
    G,
    order = igraph::vcount(G),
    nodes = y,
    mode = "all"
  )))
  co <- igraph::V(G)[connected]$name
  co <- co %ts% topo
  return(co)
}
